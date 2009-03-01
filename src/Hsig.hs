-- add type signatures to all top-level vars in a haskell file

module Hsig where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import FUtil
import HSH
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import System.IO
import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S

dropHashBang :: [[Char]] -> ([[Char]], [[Char]])
dropHashBang (l@('#':'!':_):rest) = (rest, [l])
dropHashBang rest = (rest, [])

parseVars :: Decl -> [Either (String, Int) String]
parseVars (FunBind ((Match srcLoc (Ident var) _ _ _ _):_)) =
  [Left (var, srcLine srcLoc)]
parseVars (PatBind srcLoc (PVar (Ident var)) _ _ _) =
  [Left (var, srcLine srcLoc)]
parseVars (TypeSig _ ((Ident v):_) _) = [Right v]
parseVars _ = []

getDecls :: ParseResult Module -> [Decl]
getDecls (ParseOk (Module _ _ _ _ _ _ decls)) = decls
getDecls x = error $ "Unknown parse result: " ++ show x

getModuleName :: ParseResult Module -> ModuleName
getModuleName (ParseOk (Module _ moduleName _ _ _ _ _)) = moduleName
getModuleName x = error $ "Unknown parse result: " ++ show x

-- do i even need nub
getVars :: [Decl] -> [Either (String, Int) String]
getVars = nub . concatMap parseVars

eithersSplit :: [Either a a1] -> ([a], [a1])
eithersSplit [] = ([], [])
eithersSplit ((Left x):rest) = first (x:) $ eithersSplit rest
eithersSplit ((Right x):rest) = second (x:) $ eithersSplit rest

onInit :: ([a] -> [a]) -> [a] -> [a]
onInit f l = (f $ init l) ++ [last l]

doSig :: ([Char], [Char]) -> ModuleName -> [[Char]] -> [([Char], Int)] ->
  IO [[Char]]
doSig (fDir, fName) (ModuleName moduleName) ls funcLines = do
  let
    file = fDir ++ "/" ++ fName
  os <- mapM (\ (func, _) -> do
    o <- run $
      echo (":t " ++ moduleName ++ "." ++ func) -|-
      ("ghci", ["-v0", "-w", "-i" ++ fDir, file])
    let
      sig = init o
      Just (name, funcType) = breakOnSubl " :: " sig
      funcType' = intercalate " " . map (dropWhile isSpace) $ lines funcType
    return (reversify (takeWhile (/= '.')) name, funcType')
    ) funcLines
  let
    (names, funcTypes) = unzip os
    lineNum = minimum $ map snd funcLines
    newSigParts = onInit (map (++ ",")) (reverse names) ++ ["::"] ++
      onInit (map (++ "->")) (breaksOnSubl "-> " $ head funcTypes)
    newSig = intercalate "\n" $
      assembleToLimit "" 80 ("" : repeat "  ") " " newSigParts
    assembleToLimit :: String -> Int -> [String] -> String -> [String] ->
      [String]
    assembleToLimit accum _n _prefixes _glue [] = [accum]
    assembleToLimit [] n (prefix:prefixes) glue (x:xs) =
      assembleToLimit (prefix ++ x) n prefixes glue xs
    assembleToLimit accum n prefixes glue (x:xs) = if length accum' > n
      then accum : assembleToLimit [] n prefixes glue (x:xs)
      else assembleToLimit accum' n prefixes glue xs
      where
      accum' = accum ++ glue ++ x
  when (length (group funcTypes) /= 1) $
    -- fixme more info in error
    error "Adjacent functions have unmatching types."
  return $ take (lineNum - 1) ls ++ [newSig] ++ drop (lineNum - 1) ls

addSigs :: ([Char], [Char]) -> IO Bool
addSigs (fDir, fName) = do
  let file = fDir ++ "/" ++ fName
  c <- readFileStrict file
  let
    (ls, header) = dropHashBang $ lines c
    parse = parseModule $ unlines ls
    decls = getDecls parse
    vars = getVars decls
    (varAll, varSig) = first M.fromList $ eithersSplit vars
    varUnsig = reverse . sortBy (compare `on` snd) . M.toList $
      foldr M.delete varAll varSig
    -- combine adjacent one-liners (maybe do more combination later)
    varsGrouped = groupByAdj ((\ x y -> x == y + 1) `on` snd) .  reverse .
      sortBy (compare `on` snd) $ M.toList varAll
    varUnsigGrouped = groupBy ((==) `on`
      (\ x -> filter (x `elem`) varsGrouped)) varUnsig
    moduleName = getModuleName parse
  ls' <- foldM (doSig (fDir, fName) moduleName) ls varUnsigGrouped
  writeFile file . unlines $ header ++ ls'
  hPutStrLn stderr "Checked type-sigs"
  return True
