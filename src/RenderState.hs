{-|
This module defines the board. A board is an array of CellType elements indexed by a tuple of ints: the height and width.

for example, The following array represents a 3 by 4 board (left top corner is (1,1); right bottom corner is (3,4)) with a snake at 
(2, 2) and (2, 3) and an apple at (3,4)

< ((1,1) : Empty), ((1,2) : Empty), ((1,3) : Empty),     ((1,2) : Empty)
, ((2,1) : Empty), ((2,2) : Snake)  ((2,3) : SnakeHead)  ((2,4) : Empty)
, ((3,1) : Empty), ((3,2) : Empty), ((3,3) : Empty),     ((3,4) : Apple) >

Which would look like this:

- - - -
- 0 $ -
- - - X


-}
module RenderState where

-- This are all imports you need. Feel free to import more things.
import Data.Array ( (//), listArray, Array )
import Data.ByteString.Builder qualified as B
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.State.Strict (MonadState, StateT, gets, modify')
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Foldable ( foldl', traverse_ )
import System.IO (stdout)

-- A point is just a tuple of integers.
type Point = (Int, Int)

-- | Cell types. We distinguish between Snake and SnakeHead
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

-- | The board info is just a description of height and width.
data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType     -- ^The board is an Array indexed by points with elements of type CellType

-- | A delta is a small change in the board at some points. For example [((2,2), SnakeHead), ((2,1), Empty)]
--   would represent the change "cell (2,2) should change to become the SnakeHead and cell (2,1) should change by an empty cell"
type DeltaBoard = [(Point, CellType)]

-- | The render message represent all message the GameState can send to the RenderState
--   Right now Possible messages are a RenderBoard with a payload indicating which cells change
--   or a GameOver message.
data RenderMessage
  = RenderBoard DeltaBoard
  | GameOver
  | UpdateScore Int
  deriving (Eq, Show)

-- | The RenderState contains the board and if the game is over or not.
data RenderState = RenderState
  { board :: Board
  , gameOver :: Bool
  , score :: Int
  } deriving (Eq, Show)

newtype RenderStep m a = RenderStep {runRenderStep :: ReaderT BoardInfo (StateT RenderState m) a}
  deriving (Functor, Applicative, Monad, MonadState RenderState, MonadReader BoardInfo)

-- This is the class of type which you can access a field of type RenderState.
class HasRenderState state where
  getRenderState :: state -> RenderState
  setRenderState :: state -> RenderState -> state

class HasBoardInfo env where
  getBoardInfo :: env -> BoardInfo


-- | Given The board info, this function should return a board with all Empty cells
emptyGrid :: BoardInfo -> Board
emptyGrid (BoardInfo h w) = listArray ((1,1), (h,w)) (replicate (h*w) Empty)


-- | Given BoardInfo, initial point of snake and initial point of apple, builds a board
buildInitialBoard
  :: BoardInfo -- ^ Board size
  -> Point     -- ^ initial point of the snake
  -> Point     -- ^ initial Point of the apple
  -> RenderState
buildInitialBoard binf sp ap =
  RenderState (emptyGrid binf // [(sp, SnakeHead), (ap, Apple)]) False 0


-- | Given tye current render state, and a message -> update the render state
updateRenderState :: (MonadReader env m, HasBoardInfo env, MonadState s m, HasRenderState s) => RenderMessage -> m ()
updateRenderState message = do
  gs0 <- gets getRenderState
  let gs1 = case message of
        GameOver -> gs0 { gameOver = True }
        RenderBoard db -> gs0 { board = board gs0 // db }
        UpdateScore usc -> gs0 { score = score gs0 + usc }
  modify' $ \s -> setRenderState s gs1


-- | Provisional Pretty printer
--   For each cell type choose a string to representing. 
--   a good option is
--     Empty -> "- "
--     Snake -> "0 "
--     SnakeHead -> "$ "
--     Apple -> "X "
--   In other to avoid shrinking, I'd recommend to use some charachter followed by an space.
ppCell :: CellType -> B.Builder
ppCell Empty      = "- "
ppCell Snake      = "0 "
ppCell SnakeHead  = "$ "
ppCell Apple      = "X "

ppScore :: Int -> B.Builder
ppScore s = "score: " <> B.intDec s <> B.charUtf8 '\n'

-- | convert the RenderState in a String ready to be flushed into the console.
--   It should return the Board with a pretty look. If game over, return the empty board.
renderStep :: (MonadReader env m, HasBoardInfo env, MonadState s m, HasRenderState s) => m B.Builder
renderStep = do
  w <- asks (width . getBoardInfo)
  RenderState b gOver s <- gets getRenderState
  let go (!str, !i) x =
        if i==w
          then (str <> ppCell x <> B.charUtf8 '\n', 1)
          else (str <> ppCell x, i+1)
      (boardString, _) = foldl' go (mempty, 1) b
  return $ if gOver
      then boardString <> ppScore s <> "game over\n"
      else boardString <> ppScore s

render :: (MonadReader env m, HasBoardInfo env, MonadState state m, HasRenderState state, MonadIO m) => m ()
render = do
  liftIO $ putStr "\ESC[2J" --This cleans the console screen
  str <- renderStep
  liftIO $ B.hPutBuilder stdout str

updateMessages :: (MonadReader env m, HasBoardInfo env, MonadState s m, HasRenderState s) => [RenderMessage] -> m ()
updateMessages = traverse_ updateRenderState

