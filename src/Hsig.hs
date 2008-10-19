-- add type signatures to all top-level vars in a haskell file

module Hsig where

import Control.Arrow
import Data.Function
import Data.List
import HSH
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import System.IO
import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S

dropHashBang :: [[Char]] -> (Int, [[Char]])
dropHashBang (('#':'!':_):rest) = (1, rest)
dropHashBang rest = (0, rest)

parseVars :: HsDecl -> [Either (String, Int) String]
parseVars (HsFunBind ((HsMatch srcLoc (HsIdent var) _ _ _):_)) = 
  [Left (var, srcLine srcLoc)]
parseVars (HsPatBind srcLoc (HsPVar (HsIdent var)) _ _) =
  [Left (var, srcLine srcLoc)]
parseVars (HsTypeSig _ ((HsIdent v):_) _) = [Right v]
parseVars _ = []

-- do i even need nub
getVars :: ParseResult HsModule -> [Either (String, Int) String]
getVars (ParseOk (HsModule _ _ _ _ decls)) = nub $ concatMap parseVars decls
getVars x = error $ "Unknown parse result: " ++ show x

eithersSplit :: [Either a a1] -> ([a], [a1])
eithersSplit [] = ([], [])
eithersSplit ((Left x):rest) = first (x:) $ eithersSplit rest
eithersSplit ((Right x):rest) = second (x:) $ eithersSplit rest

doSig :: (Num t) => ([Char], [Char]) -> t -> ([Char], t) -> IO ()
doSig (fDir, fName) linesOffset (func, line) = do
  let file = fDir ++ "/" ++ fName
  -- fixme: just rewrite to insert lines in haskell?
  -- fixme: must be able to use HSH.ShellEquivs.echo ?
  o <- run $ 
    ("echo", [":t", func]) -|- ("ghci", ["-v0", "-w", "-i" ++ fDir, file])
  -- no way to suppress stderr line count-ery from ed with HSH..
  runIO $ ("echo",
    [show (line + linesOffset) ++ "i\n" ++ o ++ ".\nw"]) -|- ("ed", [file])

addSigs :: ([Char], [Char]) -> IO Bool
addSigs (fDir, fName) = do
  let file = fDir ++ "/" ++ fName
  c <- readFile file
  let
    (linesOffset, lines') = dropHashBang $ lines c
    (varAll, varSig) = first M.fromList . eithersSplit . 
      getVars . parseModule $ unlines lines'
    varUnsig = reverse . sortBy (compare `on` snd) . M.toList $
      foldr M.delete varAll varSig
  mapM_ (doSig (fDir, fName) linesOffset) varUnsig
  hPutStrLn stderr "Checked type-sigs"
  return True
