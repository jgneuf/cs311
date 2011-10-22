-- Game.hs
--
-- NOTE: This is not an example of good Haskell code.  Please do not emulate.
--

module Game (GameState, nextStates, initialState, Player (PlayerA, PlayerB), gameValue, getPlayer, simulateGame, simulateAIGame, GameValue) where

import Control.Exception
import Data.Maybe

data Player = PlayerA | PlayerB deriving Eq
data PlayerState = PlayerState Player Int [Int]
data GameState = GameState PlayerState PlayerState
data GameValue = BWin | Dif Int | AWin deriving (Eq, Ord)

instance Show Player where
  show PlayerA = "A"
  show PlayerB = "B"

instance Show PlayerState where
  show (PlayerState player score pits) =
	(show player) ++ ": " ++ (show score) ++ "; " ++ (show pits)
instance Show GameValue where
	show BWin = "-Inf"
	show (Dif a) = show a
	show AWin = "Inf"



instance Show GameState where
  show s =
    "| A | " ++ showpits (reverse (getPits PlayerA s)) ++ "|" ++ threechars (' ':(show (getScore PlayerB s))) ++ "|\n" ++
    "|" ++ threechars (' ':((show (getScore PlayerA s)))) ++ "| " ++ showpits (getPits PlayerB s) ++ "| B |" where
    showpits [] = ""
    showpits (p:ps) = (threechars (show p)) ++ (showpits ps)
    threechars s@(c1:c2:c3:cs) = s
    threechars s = threechars (s ++ " ")

otherPlayer :: Player -> Player
otherPlayer PlayerA = PlayerB
otherPlayer PlayerB = PlayerA

initialState :: Player -> GameState
initialState p = 
  GameState (PlayerState p 0 (replicate 6 4)) (PlayerState (otherPlayer p) 0 (replicate 6 4))

getScore :: Player -> GameState -> Int
getScore p (GameState (PlayerState a sa _) (PlayerState b sb _)) =
  if p == a then sa else sb

gameValue :: GameState -> GameValue
gameValue s = case winnerOf s of
				   Just PlayerA -> AWin 
				   Just PlayerB -> BWin 
				   Nothing -> Dif $ (getScore PlayerA s) - (getScore PlayerB s) 

getPits :: Player -> GameState -> [Int]
getPits p (GameState (PlayerState a _ pa) (PlayerState b _ pb)) =
  if p == a then pa else pb

getPlayer :: GameState -> Player
getPlayer (GameState (PlayerState p _ _) _) = p

incStones :: PlayerState -> PlayerState
incStones p = distStones [1..6] p

incStonesBy :: Int -> PlayerState -> PlayerState
incStonesBy n p = (iterate incStones p) !! n

incScore :: Int -> PlayerState -> PlayerState
incScore i (PlayerState p s ps) = (PlayerState p (s + i) ps)

distStones :: [Int] -> PlayerState -> PlayerState
distStones is (PlayerState p s ps) =
  (PlayerState p s (map (\ (i, n) -> if (elem i is) then n + 1 else n) (zip [1..6] ps)))

winnerOf :: GameState -> Maybe Player
winnerOf s@(GameState (PlayerState pa a _) (PlayerState pb b _)) | a >= 24 || b >= 24 = Just $ if a > b then pa else pb
															 | otherwise = Nothing

isMoveValid :: Int -> Bool
isMoveValid n = elem n [1..6]

isMoveLegal :: GameState -> Int -> Bool
isMoveLegal _ n | not (isMoveValid n) = False
isMoveLegal s _ | (isJust (winnerOf s)) = False
isMoveLegal s n = case s of
                    (GameState (PlayerState a sa pa) _) -> not $ (pa !! (n - 1)) == 0

-- Given a game state and a cup, return the next state if the player chooses
-- that cup.  If choosing that cup is illegal, returns Nothing.
applyMove :: GameState -> Int -> Maybe GameState
applyMove s n | not (isMoveLegal s n) = Nothing
applyMove s n = Just (applyMove' s' p')
  where
    (s', p') = case s of
                (GameState (PlayerState a sa pa) playerother) ->
                  ((GameState (PlayerState a sa (killPit pa)) playerother), pa !! (n - 1))
                  
    killPit xs = (take (n - 1) xs) ++ [0] ++ (drop n xs)
    
    -- Given a state and a number of stones from the cup, distribute them.
    applyMove' (GameState a b) p = 
      let (rounds, extras) = divMod p 13
          sinc             = rounds + (if (6 - n) < extras then 1 else 0)
          extras' = if n == 6
                      then extras - 1
                      else if null adist then 0 else extras - (last adist) + (head adist) - 2
          extras''  = if null bdist then 0 else extras' - (last bdist) + (head bdist) - 1
          adist     = [(n + 1)..(min 6 (extras+n))]
          bdist     = [1..(min extras' 6)]
          adist'    = [1..(min extras'' 6)]
          currP = ((incScore sinc) . (distStones adist) . (distStones adist') . (incStonesBy rounds) $ a)
          otherP = ((distStones bdist) . (incStonesBy rounds) $ b) in
	case if extras == (6 - n + 1) then (GameState currP otherP) else (GameState otherP currP) of
		GameState psa@(PlayerState pa sa [0,0,0,0,0,0]) (PlayerState pb sb cb) ->
			GameState psa (PlayerState pb (sb + (sum cb)) [0,0,0,0,0,0])
		GameState (PlayerState pa sa ca) psb@(PlayerState pb sb [0,0,0,0,0,0]) ->
			GameState (PlayerState pa (sa + (sum ca)) [0,0,0,0,0,0]) psb
		s -> s


-- Returns each possible move (1 to 6) and the corresponding resulting state
nextMoves :: GameState -> [(GameState, Int)]
nextMoves s = [(s, i) | (i, (Just s)) <- zip [1..6] (map (applyMove s) [1..6])]

nextStates = (map fst) . nextMoves

-- PATCH:
-- IF errorCalls IS CAUSING AN ERROR FOR YOU, UNCOMMENT THESE LINES!
-- errorCalls (ErrorCall _) = Just ()
-- errorCalls _             = Nothing

getMove :: GameState -> IO Int
getMove st@(GameState (PlayerState p _ _) _) = do
      print st
      putStr ((show p) ++ ": ")
      handleJust errorCalls (\_ -> do
		putStrLn "Invalid move"
		getMove st)
	     (do
	     	ln <- getLine
		evaluate (read ln) :: IO Int)


simulateGame :: IO ()
simulateGame = simulateGame' (Just (initialState PlayerA)) where
  simulateGame' Nothing = return ()
  simulateGame' jst@(Just st@(GameState (PlayerState p s ps) b)) =
    do
      move <- getMove st
      s' <- return (applyMove (GameState (PlayerState p s ps) b) move)
      case s' of
        Nothing -> do
	 	putStrLn $ "Invalid move " ++ (show move)
	 	simulateGame' jst
        (Just a) -> case winnerOf a of
                          Just winner ->  print a >> putStr ((show winner) ++ " wins!\n")
                          Nothing -> simulateGame' s'
      return ()

maxfst (t:ts) = maxfst' t ts where
	maxfst' m [] = m
	maxfst' m (t:ts) = if (fst m) < (fst t) then maxfst' t ts else maxfst' m ts

simulateAIGame :: (Ord a, Show a) => (GameState -> a) -> IO ()
simulateAIGame chooseMove = simulateGame' (Just (initialState PlayerA)) where
  simulateGame' Nothing = return ()
  simulateGame' jst@(Just st@(GameState (PlayerState p s ps) b)) =
    do
      move <-
	  if p == PlayerB then
	  	getMove st
	  else do
		print st
		putStr ((show p) ++ ": ")
	  	let (states, cups) = unzip $ nextMoves st
	  	let scores = map chooseMove states
		let moves = zip scores cups
		let mv = snd $ maxfst moves
		putStrLn (show mv)
		putStrLn (show moves)
		return mv
      s' <- return (applyMove (GameState (PlayerState p s ps) b) move)
      case s' of
        Nothing -> if p == PlayerA then putStr "AI ERROR\n" else do
	 	putStrLn $ "Invalid move " ++ (show move)
	 	simulateGame' jst
        (Just a) -> case winnerOf a of
                          Just winner ->  print a >> putStr ((show winner) ++ " wins!\n")
                          Nothing -> simulateGame' s'
      return ()