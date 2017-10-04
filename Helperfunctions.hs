module Helperfunctions where
import Datatypes
import Data.List
import Data.Maybe


-- | Least fixpoint of an IO function
lfpIO :: Eq a => (a -> IO a) -> a -> IO a
lfpIO f x = do
  y <- f x
  if x == y
      then return y
      else lfpIO f y

update :: Field -> Value -> Packet -> Packet
update f v p = [ (g, if g==f then v else w) | (g,w) <- p ]

mergeField:: Field -> Field -> Packet -> Packet
mergeField f i p = newPacket where
  newPacket = [ (g,  if g==f then LS (sort $ nub $ whatValue (p ! i) ++ whatValue w) else w) | (g,w) <- p ]

whatValue :: Value -> [Switch]
whatValue (LS w)  = w
whatValue (P _)   = error "not the right type"
whatValue (S _)   = error "not the right type"
whatValue (ST _)  = error "not the right type"

whatValueString :: Value -> String
whatValueString (LS _) = error "not the right type"
whatValueString (P _)  = error "not the right type"
whatValueString (S _)  = error "not the right type"
whatValueString (ST w) = w

lfp :: Eq a => (a -> a) -> a -> a
lfp f x | x == f x  = x
        | otherwise = lfp f (f x)

(!) :: Eq a => [(a,b)] -> a -> b
(!) rel x = fromJust $ lookup x rel

makeString:: Field -> Switch -> String
makeString f x = f ++ show x

-- | Indent with three spaces
indent :: String
indent = "   "

isUnique:: String -> [String] -> Bool
isUnique _ []      = True
isUnique x (v:vs) | myPrefix x v = False
                  | otherwise    = isUnique x vs

myPrefix :: String -> String -> Bool
myPrefix [] [] = False
myPrefix [] _  = True
myPrefix _ []  = False
myPrefix (x:xs) (y:ys) = (x == y) && myPrefix xs ys

-- simplifying policies
simplify :: Policy -> Policy
simplify f = if simStep f == f then f else simplify (simStep f)

simStep :: Policy -> Policy
simStep (Filter predicate) = Filter predicate
simStep (Mod field value)  = Mod field value
simStep (PSeq [])          = Filter One
simStep (PSeq [f])         = simStep f
simStep (PSeq fs)          | Filter Zero `elem` fs = Filter Zero
                           | otherwise     = PSeq (nub $ map simStep (filter (Filter One /=) fs))
simStep (PCup [])          = Filter Zero
simStep (PCup [f])         = simStep f
simStep (PCup fs)          | Filter One `elem` fs = Filter One
                           | otherwise     = PCup (nub $ map simStep (filter (Filter Zero /=) fs))
simStep (Star f)           = Star (simStep f)
simStep (Merge f s)        = Merge f s
simStep (Call s w)         = Call s w
