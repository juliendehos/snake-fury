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
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.State.Strict (State, get, runState, modify')
import Control.Monad.Trans (lift)
import Data.Foldable ( foldl', traverse_ )

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

type RenderStep a = ReaderT BoardInfo (State RenderState) a

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
updateRenderState :: RenderMessage -> RenderStep ()
updateRenderState GameOver = lift $ modify' (\gs -> gs { gameOver = True })
updateRenderState (RenderBoard db) = lift $ modify' (\gs -> gs { board = board gs // db })
updateRenderState (UpdateScore usc) = lift $ modify' (\gs -> gs { score = score gs + usc })


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
renderStep :: [RenderMessage] -> RenderStep B.Builder
renderStep messages = do
  updateMessages messages
  w <- asks width
  RenderState b gOver s <- lift get
  let go (!str, !i) x =
        if i==w
          then (str <> ppCell x <> B.charUtf8 '\n', 1)
          else (str <> ppCell x, i+1)
      (boardString, _) = foldl' go (mempty, 1) b
  return $ if gOver
      then boardString <> ppScore s <> "\ngame over"
      else boardString <> ppScore s

render :: [RenderMessage] -> BoardInfo -> RenderState ->  (B.Builder, RenderState)
render messages bi = runState (runReaderT (renderStep messages) bi)

updateMessages :: [RenderMessage] -> RenderStep ()
updateMessages = traverse_ updateRenderState


