module Laziness (divides, isPrime, isPrime2, primes, {-mkTree, mancala, prune, minimax, level-}) where

--import Game
import Test.HUnit

-- PROBLEM 1

-- divides x n is True iff x evenly divides n.
divides :: Integer -> Integer -> Bool
divides x y = y `rem` x == 0

-- Tests for divides, simple stuff.
divTests = test [ "div1" ~: "(divides 2 4)" ~: (divides 2 4) ~=? True,
	   	  "div2" ~: "(divides 3 4)" ~: (divides 3 4) ~=? False,
          "div3" ~: "(divides 1 2)" ~: (divides 1 2) ~=? True]

-- isPrime n is True iff n is a prime number
isPrime :: Integer -> Bool
isPrime n 
    | n <= 0    = False
    | otherwise = length [x | x <- [1..n], x `divides` n] == 2

-- Tests for isPrime, simple stuff.
ipTests = test [ "isPrime1" ~: "(isPrime 2)" ~: (isPrime 2) ~=? True,
	       	 "isPrime2" ~: "(isPrime 3)" ~: (isPrime 3) ~=? True,
             "isPrime3" ~: "(isPrime 4)" ~: (isPrime 4) ~=? False,
             "isPrime4" ~: "(isPrime 5)" ~: (isPrime 5) ~=? True,
             "isPrime5" ~: "(isPrime 6)" ~: (isPrime 6) ~=? False,
             "isPrime6" ~: "(isPrime 7)" ~: (isPrime 7) ~=? True,
             "isPrime7" ~: "(isPrime 8)" ~: (isPrime 8) ~=? False]

-- primes is a list of all the prime numbers
primes :: [Integer]
primes = [x | x <- [2..], isPrime x]

-- isPrime2 n is True iff n is a prime number, but it uses primes
isPrime2 :: Integer -> Bool
isPrime2 n
    | n <= 1    = False
    | otherwise = n `elem` integerTake n primes

-- Tests for isPrime2, the same as for isPrime.
ip2Tests = test [ "isPrime2-1" ~: "(isPrime 2)" ~: (isPrime2 2) ~=? True,
	       	 "isPrime2-2" ~: "(isPrime2 3)" ~: (isPrime2 3) ~=? True,
             "isPrime2-3" ~: "(isPrime2 4)" ~: (isPrime2 4) ~=? False,
             "isPrime2-4" ~: "(isPrime2 5)" ~: (isPrime2 5) ~=? True,
             "isPrime2-5" ~: "(isPrime2 6)" ~: (isPrime2 6) ~=? False,
             "isPrime2-6" ~: "(isPrime2 7)" ~: (isPrime2 7) ~=? True,
             "isPrime2-7" ~: "(isPrime2 8)" ~: (isPrime2 8) ~=? False]

-- integerTake returns a list of length n using the given list. Haskell's take
-- uses argument of type Int, this takes Integer.
integerTake :: Integer -> [a] -> [a]
integerTake 0 _      = []
integerTake _ []     = []
integerTake n (x:xs) = x : integerTake (n - 1) xs

-- Tests for integerTake.
myitTests = test [ "integerTake1" ~: "(integerTake 0 [])" ~: (integerTake 0 "") ~=? "",
	       	 "integerTake2" ~: "(integerTake 0 [1..5])" ~: (integerTake 0 [1..5]) ~=? [],
             "integerTake3" ~: "(integerTake 2 [1..5])" ~: (integerTake 2 [1..5]) ~=? [1,2],
             "integerTake4" ~: "(integerTake 5 [1..5])" ~: (integerTake 5 [1..5]) ~=? [1..5],
             "integerTake5" ~: "(integerTake 10 [2])" ~: (integerTake 10 [2]) ~=? [2]]

{-
-- PROBLEM 2

-- NOTE: it may be harder to build good test cases here.  Try to do so
-- at least for prune and minimax!

-- This is the GameTree datatype you should define.
-- Remember: "Each node should have its current configuration 
-- and a list of trees, where each tree corresponds to the game 
-- states obtainable after making any one legal move."
data GameTree = []

-- mkTree s yields the complete GameTree whose root has state s
mkTree :: GameState -> GameTree


-- mancala is the entire game of mancala as a GameTree.  Note that PlayerA
-- goes first.
mancala :: GameTree


-- prune n gt with n > 0 yields a GameTree equivalent to gt up to depth n,
-- but with no subtrees below depth n.
-- Note: prune 0 gt is nonsense; do not provide an equation for prune 0 gt.
prune :: Int -> GameTree -> GameTree

-- NOTE: you may need to replace "GameTree" in this test with something else
-- to make it work, depending on how you defined your data type!
pruneTests = test [ "total-prune" ~: "(prune 1 mancala)" ~: 
	   (null (case (prune 1 mancala) of GameTree s ts -> ts)) ~=? True ]

-- minimax gt yields the minimaxed gameValue of the given GameTree.  The value
-- of a node with no children is the gameValue of its GameState.  
-- If the node has children and it's PlayerA's turn, then A can choose the
-- child state with maximum value.  If the node has no children and it's 
-- PlayerB's turn, then B can choose the child state with minimum value.
-- Together, these rules define the value of any (finite) game tree.
minimax :: GameTree -> GameValue


-- level n simulates a game of mancala such that PlayerA (who goes first) is
-- controlled by your minimax AI, with n levels of lookahead.  We have written
-- this one for you.
level :: Int -> IO ()
level n = simulateAIGame (minimax . (prune n) . mkTree)

-}

-- All tests.
tests = TestList [divTests, ipTests, ip2Tests, myitTests]

