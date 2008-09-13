-- correct missing imports by guessing with hoogle
-- you must have hoogle installed
-- usage: himportant <source-file> <further-args-for-ghc>

module Main where

import Control.Arrow
import Data.List
import Data.Maybe
import Hsig
import HSH
import System.Directory
import System.Environment
import System.IO
import System.Process
import FUtil
import qualified Data.Map as M

-- not implemented in v1: qualified imports (for Map, Set, etc)

funcToMod :: String -> IO (Maybe String)
funcToMod k = do
  (pIn, pOut, pErr, pId) <- runInteractiveProcess "hoogle" [k] Nothing Nothing
  waitForProcess pId
  h <- hGetContents pOut
  let
    tryMod mods mod orElse = if mod `elem` mods then mod else orElse
  return $ if h == "No results found\n"
    then Nothing
    -- some heuristics are needed:
    -- Data.Function > Data.List > ByteString
    -- System.IO > ByteString
    else let
      mods = map head . filter ((== k) . (!! 1)) . map words $ lines h
      in Just $ foldr (tryMod mods) (head mods) 
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

addImports :: String -> [String] -> IO ()
addImports fPath ghcArgs = do
  (pIn, pOut, pErr, pId) <- runInteractiveProcess "ghc" (fPath:ghcArgs)
    Nothing Nothing
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
    else hPutStrLn stderr $
      "Unknown variables: " ++ intercalate "," (map fst unfound)

killSuff :: (Eq a) => a -> [a] -> [a]
killSuff c = reverse . drop 1 . dropWhile (/= c) . reverse

addDepends :: [Char] -> IO ()
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
    _ -> return ()

main :: IO ()
main = do
  args <- getArgs
  let err = error "usage"
  ((fDir, fName), ghcArgs) <- case args of
    [] -> 
      fmap (flip (,) [] . headOr err . concat) $ mapM lookForDir ["src", "."]
    f:a -> return ((".", f), a)
  let fPath = fDir ++ "/" ++ fName
  hPutStrLn stderr fPath
  addImports fPath ghcArgs
  addDepends fPath
  addSigs (fDir, fName)
