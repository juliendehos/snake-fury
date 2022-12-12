module EventQueueSpec where

import EventQueue

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "calculateSpeed" $ do

        it "0"  $ calculateSpeed 0 200_000 `shouldBe` 200_000
        it "1"  $ calculateSpeed 1 200_000 `shouldBe` 200_000
        it "9"  $ calculateSpeed 9 200_000 `shouldBe` 200_000
        
        it "10" $ calculateSpeed 10 200_000 `shouldBe` 180_000
        it "11" $ calculateSpeed 11 200_000 `shouldBe` 180_000
        it "19" $ calculateSpeed 19 200_000 `shouldBe` 180_000

        it "20" $ calculateSpeed 20 200_000 `shouldBe` 160_000
        it "21" $ calculateSpeed 21 200_000 `shouldBe` 160_000
        it "29" $ calculateSpeed 29 200_000 `shouldBe` 160_000

        it "40" $ calculateSpeed 40 200_000 `shouldBe` 120_000
        it "41" $ calculateSpeed 41 200_000 `shouldBe` 120_000
        it "49" $ calculateSpeed 49 200_000 `shouldBe` 120_000

        it "50" $ calculateSpeed 50 200_000 `shouldBe` 100_000
        it "51" $ calculateSpeed 51 200_000 `shouldBe` 100_000
        it "59" $ calculateSpeed 59 200_000 `shouldBe` 100_000

        it "60" $ calculateSpeed 60 200_000 `shouldBe` 100_000
        it "61" $ calculateSpeed 61 200_000 `shouldBe` 100_000
        it "69" $ calculateSpeed 69 200_000 `shouldBe` 100_000

        
        
