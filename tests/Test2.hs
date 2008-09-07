-- correct missing imports by guessing with hoogle
-- you must have hoogle installed
-- usage: himportant <source-file> <further-args-for-ghc>

module Main where

import qualified Data.Map as M

-- not implemented in v1: qualified imports (for Map, Set, etc)

funcToMod :: String -> IO (Maybe String)
funcToMod k = do
  (pIn, pOut, pErr, pId) <- runInteractiveProcess "hoogle" [k] Nothing Nothing
  waitForProcess pId
  h <- hGetContents pOut
  let
    tryMod mod mods orElse = if mod `elem` mods then mod else orElse
  return $ if h == "No results found"
    then Nothing
    -- some heuristics are needed:
    -- Data.List > ByteString
    -- System.IO > ByteString
    else let 
      mods = map (head . words) $ lines h 
      in Just . tryMod "Data.List" mods . tryMod "System.IO" mods $ head mods

main :: IO ()
main = do
  args <- getArgs
  let fName:_ = args
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
          (top, posttop) = 
            span (\ x -> null x || any (`isPrefixOf` x) ["--", "module "]) l
          (imps, rest) = span ("import " `isPrefixOf`) posttop
          in top ++ sort (imps ++ newImports) ++ 
            if null imps then [""] else [] ++ rest
      length c `seq` writeFile fName . (unlines . addImports) $ lines c
    else hPutStrLn stderr $ 
      "Unknown variables: " ++ intercalate "," (map fst unfound)
