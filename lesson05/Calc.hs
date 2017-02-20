{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -XDataKinds #-}

import ExprT
import Parser

-- Excercise 1

class Evaluable a where
  eval :: a -> Integer

instance Evaluable ExprT where
  eval (Lit a)   = a
  eval (Add a b) = eval a + eval b
  eval (Mul a b) = eval a * eval b

-- Excercise 2  

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . parseExp Lit Add Mul

evalMaybe :: Evaluable a => Maybe a -> Maybe Integer
evalMaybe Nothing = Nothing
evalMaybe (Just x) = Just (eval x)

-- Excercise 3

class Expr a where
  lit ::  Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit a = Lit a
  add a b = Add a b
  mul a b = Mul a b

reify :: ExprT -> ExprT
reify = id

-- Excercise 4

instance Expr Integer where
  lit a = a
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
  lit a = Mod7 a
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

-- Excercise 5

