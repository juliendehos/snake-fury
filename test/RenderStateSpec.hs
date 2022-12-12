{-# LANGUAGE OverloadedStrings #-}

module RenderStateSpec where

import RenderState

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.State.Strict (execState)
import Data.Array
import Data.ByteString.Builder qualified as B
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "emptyGrid" $ do

        it "1" $ 
            emptyGrid (BoardInfo 2 2)
            `shouldBe`
            array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]

   
    describe "buildInitialBoard" $ do

        it "1" $ 
            buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
            `shouldBe`
            RenderState 
              { board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)]
              , gameOver = False
              , score = 0
              }

    describe "updateRenderState" $ do

        let board_info = BoardInfo 2 2
            initial_board =  buildInitialBoard board_info (1,1) (2,2)
            message1 = RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), RenderState.Empty)]
            message2 = GameOver

        let rs1 = execState (runReaderT (updateRenderState message1) board_info) initial_board
        it "1" $ 
            rs1
            `shouldBe`
            RenderState 
              { board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)]
              , gameOver = False
              , score = 0
              }

        let rs2 = execState (runReaderT (updateRenderState message2) board_info) initial_board
        it "2" $ 
            rs2
            `shouldBe`
            RenderState 
              { board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)]
              , gameOver = True
              , score = 0
              }

    describe "renderStep" $ do

        let board_info = BoardInfo 2 2
            initial_board =  buildInitialBoard board_info (1,1) (2,2)
            messages = 
              [ RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), RenderState.Empty)]
              , UpdateScore 1
              ]

        let rs1 = execState (runReaderT (renderStep messages) board_info) initial_board
        it "1" $ 
            rs1
            `shouldBe`
            RenderState 
              { board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)]
              , gameOver = False
              , score = 1
              }

    describe "render" $ do

        let b = listArray ((1,1), (3,4)) 
                  [ RenderState.Empty, RenderState.Empty, RenderState.Empty, RenderState.Empty
                  , RenderState.Empty, Snake, SnakeHead, RenderState.Empty
                  , RenderState.Empty, RenderState.Empty, RenderState.Empty, Apple
                  ]
            board_info = BoardInfo 3 4
            render_state = RenderState b  False 0

        (str1, gs1) <- render [] board_info render_state
        it "1" $ 
            B.toLazyByteString str1
            `shouldBe`
            "- - - - \n- 0 $ - \n- - - X \nscore: 0\n"

