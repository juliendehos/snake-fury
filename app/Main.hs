-- cabal run snake-fury 6 8 3

{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (
  forkIO,
  threadDelay,
 )
import EventQueue (
  Event (Tick, UserEvent),
  EventQueue,
  readEvent,
  writeUserInput,
  setSpeed
 )
import GameState (GameState (movement), move, opositeMovement)
import Initialization (gameInitialization)
import RenderState (BoardInfo, RenderState (..), render, updateMessages)

import qualified Data.ByteString.Builder as B
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, hSetEcho, stdin, stdout)

-- The game loop is easy:
--   - wait some time
--   - read an Event from the queue
--   - Update the GameState
--   - Update the RenderState based on message delivered by GameState update
--   - Render into the console
gameloop :: BoardInfo -> GameState -> RenderState -> EventQueue -> IO ()
gameloop binf gstate rstate queue = do
  let s = score rstate
  new_speed <- setSpeed s queue
  threadDelay new_speed
  event <- readEvent queue
  let (delta, gstate') =
        case event of
          Tick -> move binf gstate
          UserEvent m ->
            if movement gstate == opositeMovement m
              then move binf gstate
              else move binf $ gstate{movement = m}
  let rstate' = updateMessages rstate delta
  putStr "\ESC[2J" --This cleans the console screen
  B.hPutBuilder stdout $ render binf rstate'
  gameloop binf gstate' rstate' queue

-- | main.
main :: IO ()
main = do
  -- enable reading key strokes
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  hSetBuffering stdout NoBuffering
  hSetBinaryMode stdout True

  -- Game Initializacion
  [h, w, fps] <- fmap read <$> getArgs
  let timeSpeed = 1_000_000 `div` fps -- One second is 1_000_000 microseconds, which is the unit used by GHC internally.
  (binf, gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

  -- Game Loop. We run two different threads, one for the gameloop (main) and one for user inputs.
  _ <- forkIO $ writeUserInput eventQueue
  let initialState = gameState
  gameloop binf initialState renderState eventQueue
