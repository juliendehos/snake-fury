{-# LANGUAGE TypeApplications #-}

{-|
This module handle the external events of the game. That is: the user inputs and the time. 
Is a good example about how to use a Concurrent Bounded Channel. Because we can't make concurrent
code without monads, this module is implemented. Nevertheless is worth to read and to get a feeling
on how monadic code looks.
-}

module EventQueue where

import qualified Snake
import Control.Concurrent.BoundedChan
    ( BoundedChan, tryWriteChan, tryReadChan )
import System.IO (hReady, stdin)


-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Snake.Movement

-- | the `UserInputQueue` is an asynchronous bounded channel which contains snake movements. This channel is feeded by key strokes
type UserInputQueue = BoundedChan Snake.Movement

-- | The `EventQueue` has a `TimeQueue` a `UserInputQueue` and the global speed of consumption. The speed is represented by the current speed
newtype EventQueue = EventQueue {userInput :: UserInputQueue}


-- | This an auxiliry function to read key strokes from the terminal.
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

-- --------------------------------
-- * Interface for the EventQueue * 
-- --------------------------------

-- | Writes Movement based on the key pressed. As implemented, the arrow keys. 
--   It loops forever.
writeUserInput :: EventQueue -> IO ()
writeUserInput queue@(EventQueue userqueue) = do
    c <- getKey
    case c of
      "\ESC[A" -> tryWriteChan userqueue Snake.North >> writeUserInput queue
      "\ESC[D" -> tryWriteChan userqueue Snake.West  >> writeUserInput queue
      "\ESC[C" -> tryWriteChan userqueue Snake.East  >> writeUserInput queue
      "\ESC[B" -> tryWriteChan userqueue Snake.South >> writeUserInput queue
      _   -> writeUserInput queue

-- | Reads the EventQueue, if it is empty, returns a Tick, else returns, the Movement
-- store in the queue
readEvent :: EventQueue -> IO Event
readEvent (EventQueue userqueue) = do
  mv <- tryReadChan userqueue
  case mv of
    Nothing   -> return Tick
    Just move -> return $ UserEvent move


