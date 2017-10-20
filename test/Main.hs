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
main = hspec $
  describe "Examples" $ do
    it "gossipExampleFour" $ sort (nub $ lnsSequences gossipExampleFour) `shouldBe` sort (nub $ lnsSequencesViaNetKAT gossipExampleFour)
    it "random examples" $ property $ \(ArbGG gg) -> lnsSequences gg `sortNubEqual` lnsSequencesViaNetKAT gg
