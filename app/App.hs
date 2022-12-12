module App where

import GameState (GameState, move, HasGameState(..), Event (..))
import RenderState (RenderState(..), BoardInfo, HasRenderState(..), HasBoardInfo(..), RenderMessage(..))
import RenderState qualified
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, gets, StateT, evalStateT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import EventQueue (EventQueue, readEvent, setSpeed, HasEventQueue(..))
import Control.Concurrent (threadDelay)
import Control.Monad (unless)

-- This is the new state, which glue together Game and Render states.
data AppState = AppState GameState RenderState

data Env = Env BoardInfo EventQueue

newtype App m a = App {runApp :: ReaderT Env (StateT AppState m) a}
  deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader Env, MonadIO)

-- We need to make AppState and instance of HasGameState so we can use it with functions from `GameState.hs`
instance HasGameState AppState where
  getGameState (AppState gs _) = gs
  setGameState (AppState _ rs) gs' = AppState gs' rs

-- We need to make AppState and instance of HasRenderState so we can use it with functions from `RenderState.hs`
instance HasRenderState AppState where
  getRenderState (AppState _ rs) = rs
  setRenderState (AppState gs _) = AppState gs

instance HasBoardInfo Env where
  getBoardInfo (Env binf _) = binf

instance HasEventQueue Env where
  getEventQueue (Env _ queue) = queue


class Monad m => MonadQueue m where
  pullEvent :: m Event          -- ^Pull an Event from the queue

class Monad m => MonadSnake m where
  updateGameState :: Event -> m [RenderMessage]
  updateRenderState :: [RenderMessage] -> m ()

class Monad m => MonadRender m where
  render :: m ()

instance (MonadIO m) => MonadQueue (App m) where
  pullEvent = do
    queue <- asks getEventQueue
    liftIO $ readEvent queue

instance Monad m => MonadSnake (App m) where
  updateGameState = move
  updateRenderState = RenderState.updateMessages

instance MonadIO m => MonadRender (App m) where
  render = RenderState.render

-- This set the the speed of the game on the score. Notice the constraint give access to all the components.
setSpeedOnScore :: (MonadReader env m, HasEventQueue env, MonadState state m, HasRenderState state, MonadIO m) => m Int
setSpeedOnScore = do
  s <- gets (score . getRenderState)
  queue <- asks getEventQueue
  liftIO $ setSpeed s queue

-- This is one step of the logic: read from the queue and-then update the game state and-then update the render state and-then render
gameStep :: (MonadQueue m, MonadSnake m, MonadRender m) => m ()
gameStep = pullEvent >>= updateGameState >>= updateRenderState >> render

-- The game loop implementation is provided. To pretty much can read in english.
gameloop :: (MonadQueue m, MonadSnake m, MonadRender m, MonadState state m, HasRenderState state, MonadReader env m, HasEventQueue env, MonadIO m) => m ()
gameloop = do
  w <- setSpeedOnScore 
  liftIO $ threadDelay w
  gameStep
  game_over <- gets (gameOver . getRenderState)
  unless game_over gameloop

-- Run the application as usual
run :: Env -> AppState -> IO ()
run env app = runApp gameloop `runReaderT` env `evalStateT` app

