module Main where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo))
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, threadDelay )
import System.IO (stdin, hReady, hSetBuffering, BufferMode (NoBuffering), hSetEcho)
import Control.Concurrent.BoundedChan
    ( newBoundedChan, tryReadChan, tryWriteChan )
import EventQueue ( Event(..), EventQueue(EventQueue) )


-- EventQueue utils
writeUserInput :: EventQueue -> IO ()
writeUserInput queue@(EventQueue userqueue) = do
    c <- getKey
    case c of
      "\ESC[A" -> tryWriteChan userqueue Snake.North >> writeUserInput queue
      "\ESC[D" -> tryWriteChan userqueue Snake.West  >> writeUserInput queue
      "\ESC[C" -> tryWriteChan userqueue Snake.East  >> writeUserInput queue
      "\ESC[B" -> tryWriteChan userqueue Snake.South >> writeUserInput queue
      _   -> writeUserInput queue

readEvent :: EventQueue -> IO Event
readEvent (EventQueue userqueue) = do
  mv <- tryReadChan userqueue
  case mv of
    Nothing   -> return Tick
    Just move -> return $ UserEvent move


getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

getRandomPoint :: Int -> Int -> IO Board.Point
getRandomPoint h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w)

inititalizePoints :: Int -> Int -> IO (Board.Point, Board.Point)
inititalizePoints h w = do
  (snakeInit, appleInit) <- (,) <$> getRandomPoint h w <*> getRandomPoint h w
  if snakeInit == appleInit
    then inititalizePoints h w
    else return (snakeInit, appleInit)



main :: IO ()
main = do
    -- enable reading key strokes
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    -- Game Init
    [h, w, timeSpeed] <- fmap read <$> getArgs
    (snakeInit, appleInit) <- inititalizePoints h w
    sg <- getStdGen
    let binf = BoardInfo h w
        gameState = Snake.AppState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
        board = Board.buildInitialBoard binf snakeInit appleInit
    newUserEventQueue <- newBoundedChan 3
    let eventQueue = EventQueue newUserEventQueue
    _ <- forkIO $ writeUserInput eventQueue
    gameloop gameState board timeSpeed eventQueue

  where
    gameloop :: Snake.AppState -> Board.RenderState -> Int -> EventQueue -> IO ()
    gameloop app b timeSpeed queue = do
        threadDelay timeSpeed
        event <- readEvent queue
        let (app',delta) =
              case event of
                    Tick           -> Snake.move app
                    UserEvent move ->
                      if Snake.movement app == Snake.opositeMovement move
                        then Snake.move app
                        else Snake.move $ app {Snake.movement = move}
            board' = b `Board.updateRenderState` delta
        putStr "\ESC[2J"
        putStr $ Board.render board'
        gameloop app' board' timeSpeed queue

