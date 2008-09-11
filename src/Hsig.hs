#!/usr/bin/env runhaskell

-- add type signatures to all top-level vars in a haskell file

import Control.Arrow
import Data.Function
import Data.List
import HSH
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S

dropHashBang :: [[Char]] -> [[Char]]
dropHashBang (('#':'!':_):rest) = rest
dropHashBang rest = rest

parseVars :: HsDecl -> Either (String, Int) String
parseVars (HsFunBind ((HsMatch srcLoc (HsIdent var) _ _ _):_)) = 
  Left (var, srcLine srcLoc)
parseVars (HsPatBind srcLoc (HsPVar (HsIdent var)) _ _) =
  Left (var, srcLine srcLoc)
parseVars (HsTypeSig _ ((HsIdent v):_) _) = Right v
parseVars x = error $ "Unknown parse-ery: " ++ show x

-- do i even need nub
getVars :: ParseResult HsModule -> [Either (String, Int) String]
getVars (ParseOk (HsModule _ _ _ _ decls)) = nub $ map parseVars decls
getVars x = error $ "Unknown parse result: " ++ show x

eithersSplit :: [Either a a1] -> ([a], [a1])
eithersSplit [] = ([], [])
eithersSplit ((Left x):rest) = first (x:) $ eithersSplit rest
eithersSplit ((Right x):rest) = second (x:) $ eithersSplit rest

doSig :: String -> (String, Int) -> IO ()
doSig file (func, line) = do
  -- fixme: just rewrite to insert lines in haskell?
  -- fixme: must be able to use HSH.ShellEquivs.echo ?
  o <- run $ ("echo", [":t", func]) -|- ("ghci", ["-v0", "-w", file])
  -- no way to suppress stderr line count-ery from ed with HSH..
  run $ ("echo", [show line ++ "a\n" ++ o ++ ".\nw"]) -|- ("ed", [file])

main :: IO ()
main = do
  args <- getArgs
  f <- case args of 
    [f] -> return f
    _ -> error "usage: hsig <file>"
  c <- readFile f
  let 
    (varAll, varSig) = first M.fromList . eithersSplit . 
      getVars . parseModule . unlines . dropHashBang $ lines c
    varUnsig = reverse . sortBy (compare `on` snd) . M.toList $
      foldr M.delete varAll varSig
  mapM_ (doSig f) varUnsig
