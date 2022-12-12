{- |
This module handle the external events of the game. That is: the user inputs and the time.
-}
module EventQueue where

import GameState (Movement (..))
import GameState qualified as Snake

import Control.Concurrent (
  MVar,
  swapMVar
 )
import Control.Concurrent.BoundedChan (
  BoundedChan,
  tryReadChan,
  tryWriteChan,
 )
import Control.Monad (void)
import System.IO (hReady, stdin)

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Snake.Movement

-- | the `UserInputQueue` is an asynchronous bounded channel which contains snake movements. This channel is feeded by key strokes
type UserInputQueue = BoundedChan Snake.Movement

-- | The `EventQueue` has a `UserInputQueue` and the global speed of consumption (as a mutable reference) and the initial speed of the game.
data EventQueue = EventQueue
  { -- | An asynchronous queue of movements the snake needs to do.
    userInput :: UserInputQueue
  , -- | A mutable reference to a Int. This is used for modifying the speed of the game as we play
    currentSpeed :: MVar Int
  , -- | The initial speed
    initialSpeed :: Int
  }

-- | Given the current score and the initial speed, calculates the new speed.
--   The speed is increased by 10% every 10 points, up to 50 points.
calculateSpeed :: Int -> Int -> Int
calculateSpeed score initialSpeed = 
  let inc = min 50 score `quot` 10
  in initialSpeed * (100 - 10*inc) `div` 100

{- | Given the current score and the event queue, updates the new speed and returns it.
   This action is mutable, therefore must be run in the IO mondad
-}
setSpeed :: Int -> EventQueue -> IO Int
setSpeed score EventQueue {currentSpeed, initialSpeed} =
  let speed = calculateSpeed score initialSpeed
  in swapMVar currentSpeed speed

-- In StackOverflow we trust.
-- This function reads the key strokes as a String.
-- The arrow keys correspond to the following strings
-- "\ESC[A" -> Up Arrow
-- "\ESC[D" -> Right Arrow
-- "\ESC[C" -> Left Arrow
-- "\ESC[B" -> Down Arrow
-- therefore the following code: 
--     k <- getKey
--     print $ k == "\ESC[B"
-- will print True when Down arrow is pressed
getKey :: IO String
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)

{- | This function translates key strokes to movements and push then into the queue.
 The player is free to push keys as fast a he/she can but the userqueue is bounded,
 meaning that if we push a movement to a filled queue it gets discarded.
 This is intented for the game play, If we press keys faster than the game speed
 they will be enqueued and pushed into the game with delay.

Check getKey function's comment for a hint
-}
writeUserInput :: EventQueue -> IO ()
writeUserInput q@(EventQueue {userInput}) = do
  key <- getKey
  case key of
    "\ESC[A" -> void $ tryWriteChan userInput North
    "\ESC[B" -> void $ tryWriteChan userInput South
    "\ESC[C" -> void $ tryWriteChan userInput East
    "\ESC[D" -> void $ tryWriteChan userInput West
    _ -> return ()
  writeUserInput q

-- | Read the EventQueue and generates an Event to pass to the user logic.
-- It should pass an UserEvent if the queue is not empty, otherwise a Tick
readEvent :: EventQueue -> IO Event
readEvent EventQueue {userInput} =
  maybe Tick UserEvent <$> tryReadChan userInput

