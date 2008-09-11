-- correct missing imports by guessing with hoogle
-- you must have hoogle installed
-- usage: himportant <source-file> <further-args-for-ghc>

module Main where

import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.IO
import System.Process
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
      mods = map (head . words) $ lines h
      in Just $ foldr (tryMod mods) (head mods) 
        ["Data.Function", "Data.List", "System.IO"]

headOr _ (x:_) = x
headOr e _ = e

lookForDir dir = do
  let mainF = dir ++ "/" ++ "Main.hs"
  doesDirectoryExist dir >>= \ t -> if t
    then do
      doesFileExist mainF >>= \ t -> if t
        then return [mainF]
        else do
          files <- getDirectoryContents dir
          return $ case filter (".hs" `isSuffixOf`) files of
            [file] -> [dir ++ "/" ++ file]
            _ -> []
    else return []

main :: IO ()
main = do
  args <- getArgs
  let err = error "usage"
  fName <- case args of
    [] -> fmap (headOr err . concat) $ mapM lookForDir ["src", "."]
    [fName] -> return fName
    _ -> err
  hPutStrLn stderr fName
  (pIn, pOut, pErr, pId) <- runInteractiveProcess "ghc" args Nothing Nothing
  waitForProcess pId
  errStr <- hGetContents pErr
  let
    funcs = map (init . drop 1 . dropWhile (/= '`')) .
      filter ("Not in scope: `" `isInfixOf`) $ lines errStr
  modMbList <- mapM funcToMod funcs
  let (found, unfound) = partition (isJust . snd) $ zip funcs modMbList
  if null unfound
    then do
      c <- readFile fName
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
      length c `seq` writeFile fName . (unlines . addImports) $ lines c
    else hPutStrLn stderr $
      "Unknown variables: " ++ intercalate "," (map fst unfound)
