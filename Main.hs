import Parser
import Recursion

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import GHC.IO.Handle
import GHC.IO.Handle.FD
import System.Environment

throw :: String -> Either String a
throw = Left

getDefinitions :: [String] -> [String] -> [(RefName, Function)] -> IO (Either String [(RefName, Function)])
getDefinitions [] _ defs = return (Right defs)
getDefinitions (file:rest) processed defs = do
  putStrLn $ "Loading " ++ file ++ " ..."
  re <- parseFile file
  case re of
    Left err -> return $ Left $ show err
    Right statements -> getDefinitions files' processed' defs'
      where
        importFilter (Import s) = Just s
        importFilter _ = Nothing
        defFilter (Definition s f) = Just (s, f)
        defFilter _ = Nothing
        defs' = defs ++ (mapMaybe defFilter statements)
        processed' = file:processed
        files' = (rest ++ mapMaybe importFilter statements) \\ processed'

linkFunc :: (Map.Map RefName Function) -> Function -> Either String NamedFunction
linkFunc _ (Const i j) = return $ ("", Const i j)
linkFunc _ Next = return ("", Next)
linkFunc _ (Projection i j) = return $ ("", Projection i j)
linkFunc m (Mu f) = do
  f' <- linkFunc m (snd f)
  return $ ("", Mu f')
linkFunc m (Recursion g h) = do
  g' <- linkFunc m (snd g)
  h' <- linkFunc m (snd h)
  return $ ("", Recursion g' h')
linkFunc m (Composition g h) = do
  g' <- linkFunc m (snd g)
  h' <- mapM ((linkFunc m) . snd) h
  return $ ("", Composition g' h')
linkFunc m (Ref s) = case Map.lookup s m of
  Just f -> return (s, f)
  Nothing -> throw $ "Undefined Reference: " ++ s

link :: Map.Map RefName Function -> [(RefName, Function)] -> Either String (Map.Map RefName Function)
link m [] = return m
link m ((s, f):rest) = do
  (_, f') <- linkFunc m f
  checkArity Nothing [] f'
  if Map.member s m 
    then throw $ "Duplicate Definition of " ++ s
    else link (Map.insert s f' m) rest

arityErr :: Int -> Int -> [Function] -> Either String Int
arityErr expect got at
  = throw $ "Arity Missmatch: \nExpected " 
 ++ show expect ++ ", Got " ++ show got
 ++ "\nTrace: \n" ++ (concat $ intersperse "\n" (map show $ take 5 at))

checkArity :: Maybe Int -> [Function] -> Function -> Either String Int
checkArity Nothing _ (Const _ j) = return j
checkArity (Just n) tr f@(Const _ j)
  = case () of
    _ | n /= j -> arityErr n j (f : tr)
      | otherwise -> return n
checkArity Nothing _ Next = return 1
checkArity (Just n) tr Next
  = if n == 1 
      then return 1 
      else arityErr n 1 (Next : tr)
checkArity Nothing _ (Projection _ j) = return j
checkArity (Just n) tr f@(Projection i j) 
  = case () of
    _ | n /= j -> arityErr n j (f : tr)
      | i > j || i < 1 -> throw $ "Invalid Projection: " ++ show f
      | otherwise -> return n
checkArity n tr f@(Composition g h) = do
  checkArity (Just $ length h) (f : tr) (snd g)
  (i:_) <- mapM (checkArity n (f : tr)) (map snd h)
  return i
checkArity n tr f@(Recursion g h) = do
  let n' = case n of
             Nothing -> Nothing
             Just x -> Just (x - 1)
  k <- checkArity n' (f : tr) (snd g)
  checkArity (Just $ k + 2) (f : tr) (snd h)
  return (k + 1)
checkArity n tr f@(Mu g) = do
  let n' = case n of
             Nothing -> Nothing
             Just x -> Just (x + 1)
  k <- checkArity n' (f : tr) (snd g)
  return (k - 1)

loop m = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case () of
    _ | s == ":q" -> return ()
    _ | s == ":r" -> main
      | otherwise -> do
      let cmd = do
                  (f, args) <- case parseCommand s of
                    Left err -> throw $ show err
                    Right x -> return x
                  (_, f') <- linkFunc m f
                  checkArity (Just $ length args) [] f'
                  return (f', args)
      case cmd of
        Left err -> putStrLn err
        Right (f, args) -> putStrLn $ show $ interpret f args
      loop m

main = do
  args <- getArgs
  defs <- getDefinitions args [] []
  case defs >>= (link Map.empty) of
    Left err -> putStrLn err >> loop Map.empty
    Right m -> loop m
  
