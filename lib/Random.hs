module Random where

import Test.QuickCheck
import Data.List

import Datatypes

newtype ArbGossipGraph = ArbGG GossipGraph deriving (Show,Eq,Ord)

subGraphWith :: [Switch] -> GossipGraph -> GossipGraph
subGraphWith as t = [ (i,(s `cap` as, n `cap` as)) | (i,(s,n)) <- t, i `elem` as ] where
  cap xs = filter (`elem` xs)

instance Arbitrary ArbGossipGraph where
  arbitrary = do
    let ags = map Switch [0..2]
    t <- mapM (\i -> do
      n' <- sublistOf (ags \\ [i])
      let n = sort $ i : n'
      let s = [i]
      return (i,(n,s))) ags
    return $ ArbGG t

unpack :: ArbGossipGraph -> GossipGraph
unpack (ArbGG t) = t

getRandomGossipGraph :: IO GossipGraph
getRandomGossipGraph = fmap unpack (generate arbitrary)
