{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where 

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point)
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random (uniformR, StdGen)
import Data.Maybe (isJust)

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty 
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our porpouse.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and 
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East  = West
opositeMovement West  = East


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation. 
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: BoardInfo -> GameState -> (Point, GameState)
makeRandomPoint BoardInfo {height, width} gs0 = 
  let (i, g1) = uniformR (1, height) (randomGen gs0)
      (j, g2) = uniformR (1, width) g1
  in ((i,j), gs0 { randomGen = g2})


-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake p (SnakeSeq h b) = p == h || isJust (S.elemIndexL p b)


-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead BoardInfo {height, width} GameState {snakeSeq, movement} = 
  let (i, j) = snakeHead snakeSeq
      wrap x n = ((x-1) `mod` n) + 1
  in case movement of
    North -> (wrap (i-1) height, j)
    South -> (wrap (i+1) height, j)
    East -> (i, wrap (j+1) width)
    West -> (i, wrap (j-1) width)


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, GameState)
newApple bi gs0@(GameState {snakeSeq, applePosition}) = 
  let (p1, gs1) = makeRandomPoint bi gs0
  in if inSnake p1 snakeSeq || p1 == applePosition
      then newApple bi gs1
      else (p1, gs1)


-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-- 
move :: BoardInfo -> GameState -> ([Board.RenderMessage] , GameState)
move bi gs0@(GameState (SnakeSeq head0 body0) apple0 _m _gen0)
  | inSnake head1 (snakeSeq gs0) = 
      ( [Board.GameOver]
      , gs0
      ) -- TODO move snake first ?
  | head1 == apple0 = 
      let (deltas, gs1) = extendSnake head1 bi gs0
      in ([Board.RenderBoard deltas, Board.UpdateScore 1], gs1)
  | S.null body0 = 
      ( [Board.RenderBoard [(head1, Board.SnakeHead), (head0, Board.Empty)]]
      , gs0 {snakeSeq = SnakeSeq head1 body0}
      )
  | otherwise = 
      let (deltas, gs1) = displaceSnake head1 bi gs0
      in ([Board.RenderBoard deltas], gs1)
  where
      head1 = nextHead bi gs0


extendSnake :: Point -> BoardInfo -> GameState -> (Board.DeltaBoard, GameState)
extendSnake head1 bi gs0@(GameState (SnakeSeq head0 body0) _ _ _) =
    ( [(head1, Board.SnakeHead), (head0, Board.Snake), (apple1, Board.Apple)]
    , gs0 {snakeSeq = SnakeSeq head1 (head0 S.<| body0), applePosition = apple1}
    )
    where (apple1, gs1) = newApple bi gs0


displaceSnake :: Point -> BoardInfo -> GameState -> (Board.DeltaBoard, GameState)
displaceSnake head1 bi gs0@(GameState (SnakeSeq head0 body0) _ _ _) = 
    ( [(head1, Board.SnakeHead), (head0, Board.Snake), (last0, Board.Empty)]
    , gs0 {snakeSeq = SnakeSeq head1 (head0 S.<| body1)}
    )
    where (body1 S.:|> last0) = body0
