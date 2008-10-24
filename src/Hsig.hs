-- add type signatures to all top-level vars in a haskell file

module Hsig where

import Control.Arrow
import Control.Monad
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

dropHashBang (l@('#':'!':_):rest) = (rest, [l])
dropHashBang rest = (rest, [])

parseVars :: HsDecl -> [Either (String, Int) String]
parseVars (HsFunBind ((HsMatch srcLoc (HsIdent var) _ _ _):_)) =
  [Left (var, srcLine srcLoc)]
parseVars (HsPatBind srcLoc (HsPVar (HsIdent var)) _ _) =
  [Left (var, srcLine srcLoc)]
parseVars (HsTypeSig _ ((HsIdent v):_) _) = [Right v]
parseVars _ = []

getDecls (ParseOk (HsModule _ _ _ _ decls)) = decls
getDecls x = error $ "Unknown parse result: " ++ show x

getModuleName (ParseOk (HsModule _ moduleName _ _ _)) = moduleName
getModuleName x = error $ "Unknown parse result: " ++ show x

-- do i even need nub
getVars = nub . concatMap parseVars

eithersSplit :: [Either a a1] -> ([a], [a1])
eithersSplit [] = ([], [])
eithersSplit ((Left x):rest) = first (x:) $ eithersSplit rest
eithersSplit ((Right x):rest) = second (x:) $ eithersSplit rest

doSig (fDir, fName) (Module moduleName) lines funcLines = do                      let                                                                               file = fDir ++ "/" ++ fName
  os <- mapM (\ (func, lines) -> do                                                 o <- run $                                                                        ("echo", [":t", moduleName ++ "." ++ func]) -|-
      ("ghci", ["-v0", "-w", "-i" ++ fDir, file])
    let
      [sig] = o
      Just (name, funcType) = breakOnSubl " :: " sig
    return (drop 1 $ dropWhile (/= '.') name, funcType)
    ) funcLines
  let
    (names, funcTypes) = unzip os
    lineNum = minimum $ map snd funcLines
    newSig = intercalate ", " (reverse names) ++ " :: " ++ head funcTypes
  when (length (group funcTypes) /= 1) $
    -- fixme more info in error
    error "Adjacent functions have unmatching types."
  return $ take (lineNum - 1) lines ++ [newSig] ++ drop (lineNum - 1) lines

addSigs :: ([Char], [Char]) -> IO Bool
addSigs (fDir, fName) = do
  let file = fDir ++ "/" ++ fName
  c <- readFileStrict file
  let
    (lines', header) = dropHashBang $ lines c
    parse = parseModule $ unlines lines'
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
  lines'' <- foldM (doSig (fDir, fName) moduleName) lines' varUnsigGrouped
  writeFile file . unlines $ header ++ lines''
  hPutStrLn stderr "Checked type-sigs"
  return True
