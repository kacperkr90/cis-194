{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL newList updatedFun
  where
    fun = empFun e
    newList = e : l
    updatedFun = f + fun

instance Monoid GuestList where
  mempty = GL [] 0
  GL es1 f1 `mappend` GL es2 f2 = GL (es1 ++ es2) (f1 + f2)

listFun :: GuestList -> Fun
listFun (GL _ f) = f

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2
  | listFun l1 > listFun l2 = l1
  | otherwise = l2

treeFold
  :: (Show a, Show b)
  => (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x []) = f x []
treeFold f (Node x ts) = f x (map (\t -> treeFold f t) ts)

sumFun :: Tree Employee -> Fun
sumFun = treeFold (\e rs -> foldl (+) 0 ((empFun e) : rs))

funniestEmployee :: Tree Employee -> Employee
funniestEmployee = treeFold (\e rs -> foldl (funnierEmployee) e (e : rs))

saddestEmployee :: Tree Employee -> Employee
saddestEmployee = treeFold (\e rs -> foldl (sadderEmployee) e (e : rs))

oneOfEmployees :: (Employee -> Employee -> Bool) -> Employee -> Employee -> Employee
oneOfEmployees f e1 e2
  | f e1 e2 = e1
  | otherwise = e2

funnierEmployee :: Employee -> Employee -> Employee
funnierEmployee e1 e2 = oneOfEmployees (\x1 x2 -> empFun x1 >= empFun x2) e1 e2

sadderEmployee :: Employee -> Employee -> Employee
sadderEmployee e1 e2 = oneOfEmployees (\x1 x2 -> empFun x1 <= empFun x2) e1 e2

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs (Node e ts) =