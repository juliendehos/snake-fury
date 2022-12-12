module GameStateSpec where

import GameState
import RenderState

import Data.Sequence
import System.Random
import Test.Hspec


import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, gets, StateT, evalStateT)
import Control.Monad.IO.Class (MonadIO (liftIO))

data AppState = AppState GameState RenderState

newtype App m a = App {runApp :: ReaderT BoardInfo (StateT AppState m) a}
  deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader BoardInfo, MonadIO)

instance HasBoardInfo BoardInfo where
  getBoardInfo binf = binf

instance HasGameState AppState where
  getGameState (AppState gs _) = gs
  setGameState (AppState _ rs) gs' = AppState gs' rs


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "opositeMovement" $ do

        it "1" $ opositeMovement North `shouldBe` South
        it "2" $ opositeMovement South `shouldBe` North
        it "3" $ opositeMovement East `shouldBe` West
        it "4" $ opositeMovement West `shouldBe` East

    describe "inSnake" $ do

        let snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])

        it "1" $ inSnake (1,1) snake_seq `shouldBe` True
        it "2" $ inSnake (1,2) snake_seq `shouldBe` True
        it "3" $ inSnake (1,4) snake_seq `shouldBe` False

    describe "nextHead" $ do

        let snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
            apple_pos = (2,2) 
            board_info = BoardInfo 4 4
            game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
            game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
            game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)

        it "1" $ nextHead board_info game_state1 `shouldBe` (1,4)
        it "2" $ nextHead board_info game_state2 `shouldBe` (2,1)
        it "3" $ nextHead board_info game_state3 `shouldBe` (4,1)

    describe "move" $ do

        let snake_head = (1,1)
            snake_seq1 = SnakeSeq snake_head (Data.Sequence.fromList [(1,2), (1,3)])
            apple_pos = (2,1) 
            board_info = BoardInfo 4 4
            game_state1 = GameState snake_seq1 apple_pos West (System.Random.mkStdGen 1)
            game_state2 = GameState snake_seq1 apple_pos South (System.Random.mkStdGen 1)
            game_state3 = GameState snake_seq1 apple_pos North (System.Random.mkStdGen 1)

            snake_seq2 = SnakeSeq (3,3) (Data.Sequence.fromList [(4,2), (4,3), (4,4), (3,4), (2,4), (2,3)])
            game_state4 = GameState snake_seq2 apple_pos South (System.Random.mkStdGen 1)

        let render_state = buildInitialBoard board_info snake_head apple_pos

        rs1 <- runApp (move Tick) `runReaderT` board_info `evalStateT` AppState game_state1 render_state
        it "1" $ 
            rs1
            `shouldBe`
            [RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),RenderState.Empty)]]

        rs2 <- runApp (move Tick) `runReaderT` board_info `evalStateT` AppState game_state2 render_state
        it "2" $ 
            rs2
            `shouldBe`
            [RenderBoard [((2,1),SnakeHead),((1,1),Snake),((2,4),Apple)], UpdateScore 1]

        rs3 <- runApp (move Tick) `runReaderT` board_info `evalStateT` AppState game_state3 render_state
        it "3" $
            rs3
            `shouldBe`
            [RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),RenderState.Empty)]]

        rs4 <- runApp (move Tick) `runReaderT` board_info `evalStateT` AppState game_state4 render_state
        it "4" $ 
            rs4
            `shouldBe`
            [GameOver]

-- TODO extendSnake

