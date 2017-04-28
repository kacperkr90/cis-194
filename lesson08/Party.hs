{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party
  where

import Employee
import Data.Tree
import Debug.Trace

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL newList updatedFun
  where
    fun        = empFun e
    newList    = e:l
    updatedFun = f + fun

instance Monoid GuestList where
  mempty = GL [] 0
  GL es1 f1 `mappend` GL es2 f2 = GL (es1 ++ es2) (f1 + f2)

listFun :: GuestList -> Fun
listFun (GL _ f) = f

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2
  | listFun l1 > listFun l2 = l1
  | otherwise               = l2

treeFold :: (Show a, Show b) => (b -> Tree a -> b) -> b -> Tree a -> b
treeFold f acc t@(Node _ []) = trace ("returning result " ++ show acc) (acc)
treeFold f acc (Node e (t:ts)) = trace ("current employee " ++ (show e)) (treeFold f acc (Node (rootLabel t) ts))
-- treeFold f acc t = trace ("calling foldl on with acc " ++ (show acc) ++ "and subForest " ++ (show $ subForest t) ++ "\n") (foldl f (acc) (subForest t))

funniestEmployee :: Tree Employee -> Employee
funniestEmployee tree = treeFold (\e t -> funnierEmployee e $ rootLabel t) (rootLabel tree) tree

funnierEmployee :: Employee -> Employee -> Employee
funnierEmployee e1 e2
  | f1 >= f2  = e1
  | otherwise = e2
  where
    f1 = empFun e1
    f2 = empFun e2

treeSumFun :: Tree Employee -> Fun
treeSumFun = treeFold (\s t -> s + empFun (rootLabel t)) 0
