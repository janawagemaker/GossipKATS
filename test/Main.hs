module Main where

import Data.List
import Test.QuickCheck
import Test.Hspec

import Random
import GossipNetKAT
import GossipExplicit
import Helperfunctions
import Examples

main :: IO ()
main = hspec $ do
  describe "lnsSequences == lnsSequencesViaNetKAT" $ do
    mapM_ (\(name,gg) ->
      it name $ sort (nub $ lnsSequences gg) `shouldBe` sort (nub $ lnsSequencesViaNetKAT gg)
      )
      [ ("gossipExample",gossipExample)
      , ("gossipExampleTwoBoring",gossipExampleTwoBoring)
      , ("succExample",succExample)
      , ("weakExample",weakExample)
      , ("gossipExample2",gossipExample2)
      , ("gossipExample3",gossipExample3)
      , ("gossipExampleTwo",gossipExampleTwo)
      , ("gossipExampleFour",gossipExampleFour)
      ]
    it "random examples" $ property $ \(ArbGG gg) -> lnsSequences gg `sortNubEqual` lnsSequencesViaNetKAT gg
  describe "explicit results " $ do
    it "strongSuccess succExample" $ strongSuccess succExample
    it "not strongSuccess gossipExample" $ not $ strongSuccess gossipExample
    it "weakSuccess gossipExample" $ weakSuccess gossipExample
