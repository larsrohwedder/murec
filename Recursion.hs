module Recursion (
    Function (..),
    NamedFunction,
    RefName,
    SourceFile,
    Statement (..),
    interpret
  ) where

import Data.List
import Data.Maybe
import Debug.Trace

type SourceFile = [Statement]
data Statement
  = Import String
  | Definition RefName Function
  deriving Show

type RefName = String

type NamedFunction = (String, Function)
showNamed :: NamedFunction -> String
showNamed ("", f) = show f
showNamed (s, _) = s

data Function
  = Const Int Int 
  | Next
  | Mu NamedFunction 
  | Recursion NamedFunction NamedFunction
  | Projection Int Int 
  | Composition NamedFunction [NamedFunction]
  | Ref RefName

instance Show Function where
  show (Const a b) = "C(" ++ show a ++ ", " ++ show b ++ ")"
  show Next = "N"
  show (Mu f) = "MU(" ++ showNamed f ++ ")"
  show (Recursion g h) = "R(" ++ showNamed g ++ ", " ++ showNamed h ++ ")"
  show (Projection a b) = "P(" ++ show a ++ ", " ++ show b ++ ")"
  show (Composition g [h]) = showNamed g ++ " . " ++ showNamed h
  show (Composition g h) = showNamed g ++ " . (" ++ (concat $ intersperse ", " $ map showNamed h) ++ ")"
  show (Ref s) = s

interpret :: Function -> [Int] -> Int
interpret (Const n _) _ = n
interpret Next [n] = n + 1
interpret (Projection i _) args = args !! (i - 1)
interpret (Composition g h) args = interpret (snd g) $ map ((flip interpret) args) (map snd h)
interpret (Recursion g h) (a:rst)
  | a <= 0 = (interpret $ snd g) rst
  | otherwise  = interpret (snd h) $ [interpret (Recursion g h) (a - 1 : rst), (a - 1)] ++ rst
interpret (Mu (_, f)) args = fromJust $ find (\n -> interpret f (args ++ [n]) == 0) [0 ..]

interpret f args = error $ "Non-exhaustive " ++ show f
