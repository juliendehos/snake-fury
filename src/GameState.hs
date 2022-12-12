{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point)
import qualified RenderState as Board

import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Control.Monad.State.Strict (MonadState, StateT(..), modify', gets)
import Data.Maybe (isJust)
import Data.Sequence ( Seq(..))
import Data.Sequence qualified as S
import System.Random (uniformR, StdGen)

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Movement

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

newtype GameStep m a = GameStep {runGameStep :: ReaderT BoardInfo (StateT GameState m) a}
  deriving (Functor, Applicative, Monad, MonadState GameState, MonadReader BoardInfo)

-- This is the class of type which you can access a field of type GameState.
class HasGameState state where
  getGameState :: state -> GameState
  setGameState :: state -> GameState -> state


-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East  = West
opositeMovement West  = East


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation. 
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: (MonadReader BoardInfo m, MonadState s m, HasGameState s) => m Point
makeRandomPoint = do
  BoardInfo height width <- ask
  gs0 <- gets getGameState
  let (i, g1) = uniformR (1, height) (randomGen gs0)
      (j, g2) = uniformR (1, width) g1
  modify' (\s -> setGameState s (gs0 { randomGen = g2 }))
  return (i,j)


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
newApple :: (MonadReader BoardInfo m, MonadState s m, HasGameState s) => m Point
newApple = do
  gs0@GameState{snakeSeq, applePosition} <- gets getGameState
  p1 <- makeRandomPoint
  if inSnake p1 snakeSeq || p1 == applePosition
      then newApple
      else modify' (\s -> setGameState s gs0 { applePosition = p1 }) >> return p1


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
move :: (MonadReader BoardInfo m, MonadState state m, HasGameState state) => Event -> m [Board.RenderMessage]
move event = do
  gs0 <- gets getGameState
  case event of
    Tick -> step
    UserEvent m -> do
      when (movement gs0 /= opositeMovement m) $
        modify' $ \s -> setGameState s gs0 {movement = m}
      step

step :: (MonadReader BoardInfo m, MonadState s m, HasGameState s) => m [Board.RenderMessage]
step = do
  gs0 <- gets getGameState
  bi <- ask
  let head1 = nextHead bi gs0
  if | inSnake head1 (snakeSeq gs0) -> return [Board.GameOver]
     | head1 == applePosition gs0 -> do
        deltas <- extendSnake head1
        apple1 <- newApple
        return [Board.RenderBoard (deltas <> [(apple1, Board.Apple)]), Board.UpdateScore 1]
     | otherwise -> do
        deltas <- displaceSnake head1
        return [Board.RenderBoard deltas]


extendSnake :: (MonadState s m, HasGameState s) => Point -> m Board.DeltaBoard
extendSnake head1 = do
  gs0@(GameState (SnakeSeq head0 body0) _ _ _) <- gets getGameState
  modify' $ \s -> setGameState s gs0{snakeSeq = SnakeSeq head1 (head0 S.<| body0)}
  return [(head1, Board.SnakeHead), (head0, Board.Snake)]


displaceSnake :: (MonadState s m, HasGameState s) => Point -> m Board.DeltaBoard
displaceSnake head1 = do
  gs0@(GameState (SnakeSeq head0 body0) _ _ _) <- gets getGameState
  let (body1 S.:|> last0) = body0
  if S.null body0 
    then modify' (\s -> setGameState s gs0{snakeSeq = SnakeSeq head1 body0})
      >> return [(head1, Board.SnakeHead), (head0, Board.Empty)]
    else modify' (\s -> setGameState s gs0{snakeSeq = SnakeSeq head1 (head0 S.<| body1)})
      >> return [(head1, Board.SnakeHead), (head0, Board.Snake), (last0, Board.Empty)]

