{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -XDataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where
import ExprT
import Parser
import StackVM
import VarExprT
import Data.Maybe
import qualified Data.Map as M

-- Excercise 1

class Evaluable a where
  eval :: a -> Integer

instance Evaluable ExprT where
  eval (ExprT.Lit a)   = a
  eval (ExprT.Add a b) = eval a + eval b
  eval (ExprT.Mul a b) = eval a * eval b

-- Excercise 2

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . parseExp ExprT.Lit ExprT.Add ExprT.Mul

evalMaybe :: Evaluable a => Maybe a -> Maybe Integer
evalMaybe Nothing = Nothing
evalMaybe (Just x) = Just (eval x)

-- Excercise 3

class Expr a where
  lit ::  Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit a   = ExprT.Lit a
  add a b = ExprT.Add a b
  mul a b = ExprT.Mul a b

reify :: ExprT -> ExprT
reify = id

-- Excercise 4

instance Expr Integer where
  lit a   = a
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit a
    | a > 0 = False
    | otherwise = True
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit     = MinMax
  add a b = max a b
  mul a b = min a b

instance Expr Mod7 where
  lit                   = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

testExp :: Expr a => Maybe a
testExp     = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool    = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM      = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat     = testExp :: Maybe Mod7

testProgram :: Maybe Program
testProgram = testExp :: Maybe Program

-- Excercise 5

instance Expr Program where
  lit a                                 = [PushI a]
  add [PushI a] [PushI b]               = [PushI a, PushI b, StackVM.Add]
  add [PushB a] [PushB b]               = [PushB a, PushB b, Or]
  add p1@(PushB _ : _) p2@(PushB _ : _) = p1 ++ p2 ++ [Or]
  add p1 p2                             = p1 ++ p2 ++ [StackVM.Add]
  mul [PushI a] [PushI b]               = [PushI a, PushI b, StackVM.Mul]
  mul [PushB a] [PushB b]               = [PushB a, PushB b, And]
  mul p1@(PushB _ : _) p2@(PushB _ : _) = p1 ++ p2 ++ [And]
  mul p1 p2                             = p1 ++ p2 ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

compileAndRun :: String -> Either String StackVal
compileAndRun = stackVM . fromJust .  compile

-- Excercise 6

class HasVars a where
  var :: String -> a

instance Expr VarExprT where
  lit = VarExprT.Lit
  add a b = VarExprT.Add a b
  mul a b = VarExprT.Mul a b

instance HasVars VarExprT where
  var = Var

type IntegerFromMap = (M.Map String Integer -> Maybe Integer)

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = M.lookup x

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add = performArtmeticOperation (+)
  mul = performArtmeticOperation (*)

performArtmeticOperation :: (Integer -> Integer -> Integer)
                         -> IntegerFromMap
                         -> IntegerFromMap
                         -> M.Map String Integer
                         -> Maybe Integer
performArtmeticOperation func exp1 exp2 m
  | isJust res1 && isJust res2 = Just $ func (fromJust res1) (fromJust res2)
  | otherwise = Nothing
  where
    res1 = exp1 m
    res2 = exp2 m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs func = func $ M.fromList vs
