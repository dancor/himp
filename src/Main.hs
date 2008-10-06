-- correct missing imports by guessing with hoogle
-- you must have hoogle installed
-- usage: himportant <source-file> <further-args-for-ghc>

-- not implemented in v1: qualified imports (for Map, Set, etc)

module Main where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Hsig
import HSH
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Process
import FUtil
import qualified Data.Map as M

data Options = OptImports | OptDepends | OptTypeSigs | OptBuild deriving Eq

options :: [OptDescr Options]
options = [
  Option "i" ["imports"] (NoArg OptImports) "",
  Option "d" ["depends"] (NoArg OptDepends) "",
  Option "t" ["type-signatures"] (NoArg OptTypeSigs) "",
  Option "b" ["build"] (NoArg OptBuild) ""
  ]

funcToMod :: String -> IO (Maybe String)
funcToMod k = do
  (pIn, pOut, pErr, pId) <- runInteractiveProcess "hoogle" [k] Nothing Nothing
  waitForProcess pId
  h <- hGetContents pOut
  let
    tryMod mods mod orElse = if mod `elem` mods then mod else orElse
    mods = 
      filter ((\ x -> x == k || x == "(" ++ k ++ ")") . (!! 1)) . map words $
      lines h
    mods' = map head mods
  return $ if h == "No results found\n" || null mods then Nothing else 
    -- fixme: are these still needed?
    -- some heuristics are needed:
    -- Data.Function > Data.List > ByteString
    -- System.IO > ByteString
    Just $ foldr (tryMod mods') (head mods') 
      ["Data.Function", "Data.List", "System.IO"]

headOr :: t -> [t] -> t
headOr _ (x:_) = x
headOr e _ = e

lookForDir :: FilePath -> IO [(FilePath, [Char])]
lookForDir dir = do
  let mainF = "Main.hs"
  doesDirectoryExist dir >>= \ t -> if t
    then do
      doesFileExist (dir ++ "/" ++ mainF) >>= \ t -> if t
        then return [(dir, mainF)]
        else do
          files <- getDirectoryContents dir
          return $ case filter (".hs" `isSuffixOf`) files of
            [file] -> [(dir, file)]
            _ -> []
    else return []

addImports :: String -> [String] -> IO Bool
addImports fPath ghcArgs = do
  let
    (fName, fDir) = first reverse . second reverse . break (== '/') $ 
      reverse fPath
    iArgs = "--make":map ("-i" ++) [fDir, "dist/build/autogen"]
  (pIn, pOut, pErr, pId) <- 
    runInteractiveProcess "ghc" (fPath:iArgs ++ ghcArgs) Nothing Nothing
  waitForProcess pId
  errStr <- hGetContents pErr
  let
    funcs = map (init . drop 1 . dropWhile (/= '`')) .
      filter ("Not in scope: `" `isInfixOf`) $ lines errStr
  modMbList <- mapM funcToMod funcs
  let (found, unfound) = partition (isJust . snd) $ zip funcs modMbList
  if null unfound
    then do
      hPutStrLn stderr $ "Added imports: " ++ show found
      c <- readFile fPath
      let
        newImports = nub $ map (("import " ++) . fromJust . snd) found
        addImports l = if null newImports then l else let
          (preMod, afterMod) = case break ("module " `isPrefixOf`) l of
            (all, []) -> ([], all)
            x -> x
          (top, posttop) = span (\ x -> null x || 
            any (`isPrefixOf` x) ["--", "module ", "#!"]) afterMod
          (imps, rest) = span ("import " `isPrefixOf`) posttop
          in preMod ++ top ++ sort (imps ++ newImports) ++
            (if null imps then [""] else []) ++ rest
      length c `seq` writeFile fPath . (unlines . addImports) $ lines c
      return True
    else do
      hPutStrLn stderr $
        "Unknown variables: " ++ intercalate "," (map fst unfound)
      return False

killSuff :: (Eq a) => a -> [a] -> [a]
killSuff c = reverse . drop 1 . dropWhile (/= c) . reverse

addDepends :: [Char] -> IO Bool
addDepends fPath = do
  let
    (fName, fDir) = first reverse . second reverse . break (== '/') $ 
      reverse fPath
  files <- getDirectoryContents "."
  case filter (".cabal" `isSuffixOf`) files of
    [file] -> do
      o <- run ("hbuild", ["-i" ++ fDir, killSuff '.' fName])
      let depsVers = drop 2 . dropWhile (/= ':') $ (lines o) !! 2
          deps = sort . map (killSuff '-') $ breaksOnSubl ", " depsVers
          sedEsc = concatMap (\ x -> case x of
            '/' -> "\\/"
            x -> [x])
      runIO ("sed", ["-i", file, "-e", "s/^\\(build-depends: *\\).*/\\1" ++ 
        sedEsc (intercalate ", " deps) ++ "/"])
      hPutStrLn stderr $ "Set depends: " ++ show deps
      return True
    _ -> return True

main :: IO ()
main = do
  args <- getArgs
  let 
    usage = "usage: himp [options] [file] [ghc-options]"
    doErr e = error $ e ++ usageInfo usage options
    (opts, moreArgs) = case getOpt Permute options args of
      (o, n, []) -> (o, n)
      (_, _, errs) -> doErr $ concat errs
  ((fDir, fName), ghcArgs) <- case moreArgs of
    [] -> fmap (flip (,) [] . headOr (doErr "") . concat) $ 
      mapM lookForDir ["src", "."]
    f:a -> return ((".", f), a)
  let 
    fPath = fDir ++ "/" ++ fName
    tryIfOpt opt f = when (null opts || opt `elem` opts) .
      unlessM f $ error "Step failed; aborting."
  hPutStrLn stderr fPath
  tryIfOpt OptImports $ addImports fPath ghcArgs
  tryIfOpt OptDepends $ addDepends fPath
  tryIfOpt OptTypeSigs $ addSigs (fDir, fName)
  tryIfOpt OptBuild $ HSH.runIO "cabal install" >> return True
