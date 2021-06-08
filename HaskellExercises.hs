-- http://pages.di.unipi.it/corradini/Didattica/AP-20/ESER/03/exercises_3.html
module HaskellExercises where

-- Exercise 1-A Write a function myReplicate that given an integer n and a value v 
-- returns a list of length n initialized with v, namely all elements are equal to v.
myReplicate :: (Eq t, Num t) => a -> t -> [a]
myReplicate n 0 = []
myReplicate n v = [n] ++ (myReplicate n (v - 1))

--Exercise 2-A Write a function sumOdd that given a list of integers computes the sum of the values that are odd.
sumOdd :: Integral p => [p] -> p
sumOdd [] = 0
sumOdd (x:xs) = (if odd x then x else 0) + sumOdd xs 

--Exercise 2-B Write a function sumOdd that given a list of integers computes the sum of the values that are odd.
sumLst :: Integral p => [p] -> p
sumLst [] = 0
sumLst (x:xs) = x + sumLst xs

sumOddTwo :: Integral p => [p] -> p
sumOddTwo lst = sumLst (filter odd lst)

--Exercise 9 Definition of a Tree
data IntTree = Leaf Int | Node (Int, IntTree, IntTree)

--Test Data Types let tree = Node(1,Node(2, Leaf 3, Leaf 4),Node(5, Leaf 6, Leaf 7))
tPrint :: IntTree -> [Char]
tPrint (Leaf x) = "Leaf(" ++ show x ++ ")"
tPrint (Node(x, n1, n2)) = "Node(" ++ show x ++ ", " ++ tPrint n1 ++ ", " ++ tPrint n2 ++ ")"

--Exercise 9.1 Implement tmap, a "tree version" of the map combinator. 
tMap :: (Int -> Int) -> IntTree -> IntTree
tMap f (Leaf x) = Leaf (f x)
tMap f (Node(x, n1, n2)) = Node(f x, tMap f n1, tMap f n2)

--Exercise 9.2 Using tmap implement the function succTree taking a tree t and computing a tree whose elements are the successors of the values in t.
tSucc :: IntTree -> IntTree
tSucc t = tMap succ t

--Exercise 9.3 Write a function sumSucc taking a tree t and computing the sum of the elements of succTree t.
sumSucc :: (Int p) => IntTree -> p
sumSucc (Leaf x) = succ x
sumSucc (Node(x, n1, n2)) = succ x + sumSucc n1 + sumSucc n2

--testcommit

