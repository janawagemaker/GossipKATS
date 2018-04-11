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
    -- make size 3 more probable than size 4
    n <- frequency [(10, return 3),(1, return 4)]
    let ags = map Switch $ take n [0..]
    t <- mapM (\i -> do
      n' <- sublistOf (ags \\ [i])
      let numbers = sort $ i : n'
      let secrets = [i]
      return (i,(numbers,secrets))) ags
    return $ ArbGG t

unpack :: ArbGossipGraph -> GossipGraph
unpack (ArbGG t) = t

getRandomGossipGraph :: IO GossipGraph
getRandomGossipGraph = fmap unpack (generate arbitrary)
