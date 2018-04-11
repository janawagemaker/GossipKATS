module GossipNetKAT where

import Data.List

import Datatypes
import NetKAT
import Helperfunctions

-- | Learn-New-Secrets policy for given switches.
lnsPol :: [Switch] -> Policy
lnsPol sws = simplify $ Star $ PCup [lnsCallPol sws i j | i <- sws, j <- sws, i /= j]

-- | Policy to make an LNS call from i to j
lnsCallPol :: [Switch] -> Switch -> Switch -> Policy
lnsCallPol sws i j =
  PSeq [ Filter (Test (makeStringT "S" i j) (I 0))
       , Filter (Test (makeStringT "N" i j) (I 1))
       , Add "call" $ LC [(i,j)]
       , callUpdatesPol sws i j ]

-- | Field updates for a call from i to j (same as j to i)
callUpdatesPol :: [Switch] -> Switch -> Switch -> Policy
callUpdatesPol sws i j =
  PSeq [ PSeq [ ifthen (makeStringT "S" i x) $ Mod (makeStringT "S" j x) (I 1)
              , ifthen (makeStringT "S" j x) $ Mod (makeStringT "S" i x) (I 1)
              , ifthen (makeStringT "N" i x) $ Mod (makeStringT "N" j x) (I 1)
              , ifthen (makeStringT "N" j x) $ Mod (makeStringT "N" i x) (I 1) ]
       | x <- sws ]

initialGraphPol :: [Switch] -> Policy
initialGraphPol ws = PSeq [initPolAgent x | x <- ws] where
  initPolAgent x = PSeq
    [ Filter (Test (makeStringT "N" x x) (I 1))
    , PSeq [Filter (Test (makeStringT "S" x w) (I (if w == x then 1 else 0))) | w <- ws]
    ]

stopPol :: [Switch] -> Policy
stopPol ws = PSeq [ PCup [ Filter (Test (makeStringT "N" i j) (I 0))
                         , Filter (Test (makeStringT "S" i j) (I 1)) ]
                  | i <- ws, j <- ws, i /= j ]

-- | Policy to test whether LNS was successful
successPol:: [Switch] -> Policy
successPol ws = PSeq [Filter (Test (makeStringT "S" x w) (I 1)) | x <- ws, w <- ws]

-- | Policy to test whether LNS was NOT successful
failPol:: [Switch] -> Policy
failPol ws = PCup [Filter (Neg $ Test (makeStringT "S" x w) (I 1)) | x <- ws, w <- ws]

thisGraphPol :: GossipGraph -> Policy
thisGraphPol = packetToTest . graphToPacket

packetToTest :: Packet -> Policy
packetToTest fvs = PSeq [ Filter (Test f v) | (f,v) <- fvs ]

packetsToCalls :: [Packet] -> [Sequence]
packetsToCalls = map (\ x -> whatValueLC (x ! "call"))

-- | Create NetKAT packet corresponding to given gossip graph.
graphToPacket :: GossipGraph -> Packet
graphToPacket [] = []
graphToPacket xs = ("call", LC []) : concatMap itemToFields xs where
  sws = map fst xs
  itemToFields (x,(numbers,secrets)) =
    [ ("N" ++ show x ++ show z, I (if z `elem` numbers then 1 else 0)) | z <- sws ]
    ++
    [ ("S" ++ show x ++ show z, I (if z `elem` secrets then 1 else 0)) | z <- sws ]

-- | Run policy on given packets, get call sequences from results.
runPolCalls :: [Packet] -> Policy -> [Sequence]
runPolCalls packets pol = nub $ packetsToCalls $ evalPol packets pol

-- | Run Learn-New-Secrets until it stops, return call sequences.
lnsSequencesViaNetKAT :: GossipGraph -> [Sequence]
lnsSequencesViaNetKAT gg =
  runPolCalls [graphToPacket gg] (PSeq [lnsPol sws, stopPol sws]) where
    sws = map fst gg

-- | Run Learn-New-Secrets until it stops, print un/succ callsequences.
readableSequences :: GossipGraph -> IO ()
readableSequences gg = do
  let sws = map fst gg
  let successes = runPolCalls [graphToPacket gg] (PSeq [lnsPol sws, successPol sws])
  let failures = runPolCalls [graphToPacket gg] (PSeq [lnsPol sws, stopPol sws, failPol sws])
  putStrLn $
    "Successful call sequences:\n"
    ++ intercalate "\n" (map show successes)
    ++ "\nUnsuccessful call sequences:\n"
    ++ intercalate "\n" (map show failures)
