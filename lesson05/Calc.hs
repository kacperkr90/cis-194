{-# OPTIONS_GHC -Wall #-}

import ExprT

class Evaluable a where
  eval :: a -> Integer

instance Evaluable ExprT where
	eval (Lit a)   = a
	eval (Add a b) = eval a + eval b
	eval (Mul a b) = eval a * eval b