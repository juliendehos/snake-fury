
{-|
This modules is entry point of the game. When a haskell executable is called, 
it enters the main function (as in any other language). The main function must have
type IO (), that is input/output returning nothing (). Because, in haskell we can't
implement IO code with out monads, this module is implemented for you. 
-}

module Main where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo))
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, threadDelay )
import System.IO (stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho)
import Control.Concurrent.BoundedChan
    ( newBoundedChan )
import EventQueue ( Event(..), EventQueue(EventQueue), writeUserInput, readEvent )


-- This are auxiliar functions for game initialization. It produces two random points. 

-- | Given height and width, it produces a random point in the board.
getRandomPoint :: Int -> Int -> IO Board.Point
getRandomPoint h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w)

-- | Given height and width, it produces two random points in the board
-- one for the snake head and one for the apple. If makes sure, both aren't 
-- the same point
inititalizePoints :: Int -> Int -> IO (Board.Point, Board.Point)
inititalizePoints h w = do
  (snakeInit, appleInit) <- (,) <$> getRandomPoint h w <*> getRandomPoint h w
  if snakeInit == appleInit
    then inititalizePoints h w
    else return (snakeInit, appleInit)

-- Entry point
main :: IO ()
main = do
    -- enable reading key strokes
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    -- Game Initialization. This just set up the board and the initial positions of the 
    -- snake and the apple.
    [h, w, timeSpeed] <- fmap read <$> getArgs
    (snakeInit, appleInit) <- inititalizePoints h w
    sg <- getStdGen
    let binf = BoardInfo h w
        gameState = Snake.GameState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
        board = Board.buildInitialBoard binf snakeInit appleInit
    newUserEventQueue <- newBoundedChan 3
    let eventQueue = EventQueue newUserEventQueue

    -- Game execution. We execute and asyncronous proccess for recording key strokes in the
    -- event queue and in the main thread we execute the game loop
    _ <- forkIO $ writeUserInput eventQueue
    gameloop gameState board timeSpeed eventQueue

  where
    gameloop :: Snake.GameState -> Board.RenderState -> Int -> EventQueue -> IO ()
    gameloop app b timeSpeed queue = do 
        threadDelay timeSpeed                                              -- Pause the loop for the timespeed we are running
        event <- readEvent queue                                           -- Read the event from the queue
        let (app',delta) =                                                 -- Calculate the new state and the messages to pass to the rendering state
              case event of
                    Tick           -> Snake.move app                       -- This is the case no user input is in the queue. We just move
                    UserEvent move ->                                      -- This is the case the user has move. We need to check if the user is not
                      if Snake.movement app == Snake.opositeMovement move  -- moving against the current movement of the snake, and modify the movement accordingly.
                        then Snake.move app
                        else Snake.move $ app {Snake.movement = move}
            board' = b `Board.updateRenderState` delta                     -- Update the render state
        putStr "\ESC[2J"                                                   -- Clear the screen
        putStr $ Board.render board'                                       -- print the board in the console.
        gameloop app' board' timeSpeed queue                               -- run again the loop

