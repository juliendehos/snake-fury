{-# LANGUAGE OverloadedStrings #-}

module RenderStateSpec where

import RenderState

import Data.Array
import qualified Data.ByteString.Builder as B
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

        let initial_board =  buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
            message1 = RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), RenderState.Empty)]
            message2 = GameOver

        it "1" $ 
            updateRenderState initial_board message1
            `shouldBe`
            RenderState 
              { board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)]
              , gameOver = False
              , score = 0
              }

        it "2" $ 
            updateRenderState initial_board message2
            `shouldBe`
            RenderState 
              { board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)]
              , gameOver = True
              , score = 0
              }

    describe "updateRenderState" $ do

        let initial_board =  buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
            messages = 
              [ RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), RenderState.Empty)]
              , UpdateScore 1
              ]

        it "1" $ 
            updateMessages initial_board messages
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

        it "1" $ 
            B.toLazyByteString (render board_info render_state)
            `shouldBe`
            "- - - - \n- 0 $ - \n- - - X \n"

