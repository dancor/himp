-- correct missing imports by guessing with hoogle
-- you must have hoogle installed
-- usage: himportant <source-file> <further-args-for-ghc>

-- not implemented in v1: qualified imports (for Map, Set, etc)

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import FUtil
import Hsig
import HSH
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Process
import FUtil
import qualified Data.Map as M

data Options = OptImports | OptDepends | OptDependsNuke | OptTypeSigs |
  OptBuild deriving Eq

options :: [OptDescr Options]
options = [
  Option "i" ["imports"] (NoArg OptImports) "",
  Option "d" ["depends"] (NoArg OptDepends) "",
  Option "D" ["depends-nuke-old"] (NoArg OptDependsNuke) "",
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
      ["Data.Function", "Data.List", "System.IO", "System.IO.Unsafe"]

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
    objDir = "obj"
    (fName, fDir) = first reverse . second reverse . break (== '/') $
      reverse fPath
    iArgs =
      ["--make", "-o", objDir, "-odir", objDir, "-hidir", objDir] ++
      map ("-i" ++) [fDir, "dist/build/autogen"]
  objExistAtStart <- doesDirectoryExist objDir
  unless objExistAtStart $ createDirectory objDir
  (pIn, pOut, pErr, pId) <-
    runInteractiveProcess "ghc" (fPath:iArgs ++ ghcArgs) Nothing Nothing
  waitForProcess pId
  unless objExistAtStart $ removeDirectoryRecursive objDir
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

wordComb :: Int -> [a] -> [[a]] -> [[a]]
wordComb _ _ [] = [[]]
wordComb len sp (w:ws) = wordCombCur ws w where
  wordCombCur [] curStr = [curStr]
  wordCombCur (w:ws) curStr = let curStr' = curStr ++ sp ++ w in
    if length curStr' > len
      then curStr:wordCombCur ws w
      else wordCombCur ws curStr'

addDepends :: [Char] -> Bool -> IO Bool
addDepends fPath justAdd = do
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
      c <- readFileStrict file
      let
        l = lines c
        buildDepHeader = "build-depends:"
        (preDeps, rest) = break
          ((filter (not . isSpace) buildDepHeader `isPrefixOf`) .
          map toLower . dropWhile isSpace) l
        (depsAndSpace, postDeps) =
          first (take 1 rest ++) $ break (':' `elem`) (drop 1 rest)
        depsAndInterPostSpace :: [String]
        (preSpace, depsAndInterPostSpace) = (fst *** uncurry (:)) . seqTup .
          first (span isSpace) $ uncons depsAndSpace
        (interSpace, depsAndPostSpace) = (fst *** uncurry (:)) $ seqTup .
          first (span isSpace . drop (length buildDepHeader)) $
          uncons depsAndInterPostSpace
        interSpace' = if null interSpace then " " else interSpace
        (postSpace, depsOrigStr) = bothond reverse . span (all isSpace) $
          reverse depsAndPostSpace
        depsOrig = filter (not . null) . uncalate "," .
          filter (not . isSpace) $ intercalate "," depsOrigStr
        buildDepHeaderSpace = buildDepHeader ++ interSpace'
        deps' = nub . sort $ if justAdd then deps ++ depsOrig else deps
        depsLines = zipWith (++) (map (preSpace ++) $
          buildDepHeaderSpace:
          repeat (replicate (length buildDepHeaderSpace) ' ')) .
          wordComb (79 - 1 - length buildDepHeaderSpace) " " $
          map (++ ",") (init deps') ++ [last deps']
      writeFile file . unlines $ preDeps ++ depsLines ++ postSpace ++ postDeps
      hPutStrLn stderr $ "Did depends: " ++ show deps
      return True
    _ -> return True

main :: IO ()
main = do
  args <- getArgs
  let
    usage = "usage: himp [options] [files]"
    doErr e = error $ e ++ usageInfo usage options
    (opts, moreArgs) = case getOpt Permute options args of
      (o, n, []) -> (o, n)
      (_, _, errs) -> doErr $ concat errs
  (fDir, fNames) <- case moreArgs of
    [] -> fmap (second (:[]) . headOr (doErr "") . concat) $
      mapM lookForDir ["src", "."]
    fs -> return (".", fs)
  let
    tryIfOpt def opt f = when (def && null opts || opt `elem` opts) .
      unlessM f $ error "Step failed; aborting."
    f fName = do
      let
        fPath = fDir ++ "/" ++ fName
      hPutStrLn stderr fPath
      tryIfOpt True OptImports $ addImports fPath []
      tryIfOpt True OptDepends $ addDepends fPath True
      tryIfOpt False OptDependsNuke $ addDepends fPath False
      tryIfOpt True OptTypeSigs $ addSigs (fDir, fName)
  mapM_ f fNames
  -- fixme: or build without cabal (maybe only do that when -b explicitly
  --    given?)
  tryIfOpt True OptBuild . ifM (null <$> globsOrNot ["*.cabal"])
    (HSH.runIO "cabal install" >> return True) $ return True
