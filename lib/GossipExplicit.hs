module GossipExplicit where

import Datatypes
import Helperfunctions

lnsSequences :: GossipGraph -> [Sequence]
lnsSequences gg
  | null (permittedCallsAt gg) = [[]]
  | otherwise = [ call:rest | call <- permittedCallsAt gg, rest <- lnsSequences (makeCall gg call) ]

permittedCallsAt :: GossipGraph -> [Call]
permittedCallsAt gg = [ (x,y) | (x,(nRel,sRel)) <- gg, y <- nRel, y `notElem` sRel ]

makeCall :: GossipGraph -> Call -> GossipGraph
makeCall gg (x,y) = map change gg where
  mergedRel f = f (gg ! x) `sortNubUnion` f (gg ! y)
  change (z,(nRel,sRel)) | z `elem` [x,y] = (z,(mergedRel fst,mergedRel snd))
                         | otherwise      = (z,(nRel,sRel))

makeCalls :: GossipGraph -> [Call] -> GossipGraph
makeCalls = foldl makeCall

isSolved :: GossipGraph -> Bool
isSolved gg = all (== map fst gg) (map (fst.snd) gg)

strongSuccess :: GossipGraph -> Bool
strongSuccess gg = all (isSolved . makeCalls gg) $ lnsSequences gg

weakSuccess :: GossipGraph -> Bool
weakSuccess gg = any (isSolved . makeCalls gg) $ lnsSequences gg
