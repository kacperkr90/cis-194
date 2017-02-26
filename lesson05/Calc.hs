{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -XDataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where
import ExprT
import Parser
import StackVM
import Data.Maybe

-- Excercise 1

class Evaluable a where
  eval :: a -> Integer

instance Evaluable ExprT where
  eval (Lit a)   = a
  eval (ExprT.Add a b) = eval a + eval b
  eval (ExprT.Mul a b) = eval a * eval b

-- Excercise 2

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . parseExp Lit ExprT.Add ExprT.Mul

evalMaybe :: Evaluable a => Maybe a -> Maybe Integer
evalMaybe Nothing = Nothing
evalMaybe (Just x) = Just (eval x)

-- Excercise 3

class Expr a where
  lit ::  Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit a   = Lit a
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
  lit a   = MinMax a
  add a b = max a b
  mul a b = min a b

instance Expr Mod7 where
  lit a                 = Mod7 a
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

testExp :: Expr a => Maybe a
testExp     = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7
testProgram = testExp :: Maybe Program

-- Excercise 5

instance Expr Program where
  lit a = [PushI a]
  add [PushI a] [PushI b] = [PushI a, PushI b, StackVM.Add]
  add [PushB a] [PushB b] = [PushB a, PushB b, Or]
  add p1@(PushB _ : _) p2@(PushB _ : _) = p1 ++ p2 ++ [Or]
  add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
  mul [PushI a] [PushI b] = [PushI a, PushI b, StackVM.Mul]
  mul [PushB a] [PushB b] = [PushB a, PushB b, And]
  mul p1@(PushB _ : _) p2@(PushB _ : _) = p1 ++ p2 ++ [And]
  mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

compileAndRun :: String -> Either String StackVal
compileAndRun = stackVM . fromJust .  compile
