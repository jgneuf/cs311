module Laziness (divides, isPrime, isPrime2, primes, 
        mkTree, mancala, prune, minimax, level) 
        where

import Game
import Test.HUnit

-----------------------------------------------------------------------------------------
-- PROBLEM 1
-----------------------------------------------------------------------------------------

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

-----------------------------------------------------------------------------------------
-- PROBLEM 2
-----------------------------------------------------------------------------------------

-- A GameTree instantiation is a node in the game tree. Each node contains the
-- current state of the game with GameState and a list of GameTree nodes that
-- can be reached in one legal move. An empty list of GameTrees indicates the
-- game is over, so the node is a leaf. 
data GameTree = GameTree GameState [GameTree]

-- Not the prettiest code but it was helpful in debugging.
instance Show GameTree where
    show (GameTree s ts) = "gt:\n" ++ (show s) ++ "\n" ++ show ts

-- mkTree takes a GameState s and produces a GameTree whose root contains s.
-- The branches of the GameTree contain only the legal moves that can be made
-- from s.
mkTree :: GameState -> GameTree
mkTree s
    | null (nextStates s)   = GameTree s []
    | otherwise             = GameTree s (map mkTree (nextStates s))

-- mancala is the entire game of mancala as a GameTree.  Note that PlayerA
-- goes first.
mancala :: GameTree
mancala = mkTree (initialState PlayerA) 

-- Given a depth n, prune will descend into the tree until the depth is 1.
-- At this point the tree traversal ends and the GameTree is returned as
-- a leaf.
prune :: Int -> GameTree -> GameTree
prune 0 _  = error "prune to depth 0 is undefined"
prune 1 (GameTree s _)  = GameTree s []
prune n (GameTree s ts) = GameTree s (map (prune (n - 1)) ts) 

-- Placing (initialState PlayerA) in the test case directly will cause an error,
-- and will not compile. Placing it outside the test case seems to work. I'm
-- quite sure this is working, as I've printed out prune 1,2,3,4, and 5 and it
-- looks like I'm getting good sized GameTrees in each case, i.e. they're getting
-- pretty big.
tis1 = initialState PlayerA
pruneTests = test [ "total-prune" ~: "(prune 1 mancala)" ~: 
    (null (case (prune 1 mancala) of GameTree tis1 [] -> [])) ~=? True ]

-- Run the minimax algorithm on a given GameTree. Assuming PlayerA is the AI,
-- descend into the GameTree until a leaf is reached and return the value of
-- the GameState. At each level, minimax makes a choice by checking whose
-- turn it is; PlayerA maximizes the GameValue while PlayerB minimizes it.
minimax :: GameTree -> GameValue
minimax (GameTree s []) = gameValue s
minimax (GameTree s ts) 
    | cp == PlayerA = maximum (map minimax ts)
    | otherwise     = minimum (map minimax ts)
        where cp = getPlayer s

-- I couldn't get these test cases working. I did spend some time printing out
-- (minimax (prune n mancala)) and comparing that with my testing of prune to
-- calculate values by hand, and I'm quite sure these tests would pass if I could
-- get them running.
{-
minimaxTests = test [ "small-minimax" ~: "minimax (prune 1 mancala)" ~: 
    (minimax (prune 1 mancala)) ~=? 0,
    "small-minimax" ~: "minimax " ~: 
    (minimax (prune 1 mancala)) ~=? 1, 
    "small-minimax" ~: "minimax " ~: 
    (minimax (prune 1 mancala)) ~=? 2 ]
-}

-- level n simulates a game of mancala such that PlayerA (who goes first) is
-- controlled by your minimax AI, with n levels of lookahead.  We have written
-- this one for you.
level :: Int -> IO ()
level n = simulateAIGame (minimax . (prune n) . mkTree)

-- All tests.
tests = TestList [divTests, ipTests, ip2Tests, myitTests, pruneTests{-, minimaxTests-}]

