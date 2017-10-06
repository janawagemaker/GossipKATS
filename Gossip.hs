module Gossip where

import Data.List
import Data.Maybe

import Datatypes
import NetKAT
import Helperfunctions

-- Running the LNS protocol on an input packet and a given netkat model, output not pretty
callSequence :: NetKATM -> [Packet] -> [Packet]
callSequence mo packstate = evalPol packstate (PSeq [polDistributeSimple mo,
                                                Star (PSeq [genModelSimple mo, genCallSimple mo])])

-- creating the first part of the call policy where the first packet is distributed
polDistribute :: NetKATM -> Policy
polDistribute (Mo _ _ _ ((_, []):_) _ _) = error "no ports"
polDistribute (Mo _ _ _ [] _ _) = Filter Zero
polDistribute (Mo h s k ((w,p:_):zs) e f) = PCup [PSeq [Mod "ag" (S w),
                                                        Mod "pt" (P p)],
                                                  polDistribute (Mo h s k zs e f)]

polDistributeSimple:: NetKATM -> Policy
polDistributeSimple m = simplify (polDistribute m)

-- Readable output for LNS protocol on input packet and netkat model (the function to run is showOutput)

showOutput:: NetKATM -> [Packet] -> IO ()
showOutput mo pacst = putStrLn $ filteredOutput mo pacst

filteredOutput:: NetKATM -> [Packet] -> String
filteredOutput mo pacst =
  "These are successful call sequences:\n  "
  ++ intercalate "\n  " successes
  ++ "\nThese are not successful call sequences:\n  "
  ++ intercalate "\n" failures where
    failures = nub $ failString mo outputs
    outputs = niceOutput mo pacst
    successes = nub $ successString mo outputs

niceOutput:: NetKATM -> [Packet] -> [Packet]
niceOutput mo pacst = filter (\z -> isUnique (whatValueST (z ! "call")) (makeCallList $ callSequence mo pacst)) (callSequence mo pacst)

makeCallList:: [Packet] -> [String]
makeCallList = map (\ x -> whatValueST (x ! "call"))

failString:: NetKATM -> [Packet] -> [String]
failString _                []     = []
failString (Mo h s p z e f) (x:xs) | null $ evalPol [x] (successPol s s (Mo h s p z e f))
                                        =  whatValueST (x ! "call") : failString (Mo h s p z e f) xs
                                   | otherwise = failString (Mo h s p z e f) xs

successString:: NetKATM -> [Packet] -> [String]
successString _                []     = []
successString (Mo h s p z e f) (x:xs) | null $ evalPol [x] (successPol s s (Mo h s p z e f))
                                          = successString (Mo h s p z e f) xs
                                      | otherwise = whatValueST (x ! "call") : successString (Mo h s p z e f) xs

-- A function generating a policy testing whether LNS was successful
successPol:: [Switch] -> [Switch] -> NetKATM -> Policy
successPol [] _ _ = Filter One
successPol (s:ws) x mo = PSeq [ Filter (Test (makeString "S" s) (LS x))
                              , successPol ws x mo ] -- FIXME this is not actually using mo?

-- generating pol_1 -- FIXME rename
genModel:: NetKATM -> Policy
genModel (Mo _ p _ z e _) = genForward p z e

genModelSimple:: NetKATM -> Policy
genModelSimple m = simplify (genModel m)

genForward:: [Switch] -> [(Switch, [Port])] -> Internallinks -> Policy
genForward _ _ [] = error "no links"
genForward _ [] _ = Filter Zero
genForward [] _ _ = error "no switches"
genForward t ((s,p):xs) z = PCup [genForwardSwitch t (s,p) z, genForward t xs z]

type Internallinks = [((Switch,Port),(Switch,Port))]

decide:: Switch -> Switch -> Internallinks -> Policy
decide _ _ [] = Filter Zero
decide s t (((h,w),(i,o)):zs) | (h == s) && (i == t) = Mod "pt" (P w)
                              | otherwise =
                                if (i == s) && (h == t)
                                  then Mod "pt" (P o)
                                  else decide s t zs

genForwardSwitch:: [Switch] -> (Switch, [Port]) -> Internallinks -> Policy
genForwardSwitch ws (s,p) z = loop ws where
  loop [] = Filter Zero
  loop (v:vs) | v == s    = loop vs
              | otherwise = PCup [ PSeq [ Filter (Test "ag" (S s) )
                                        , Filter (Test "pt" (P  (head p)))
                                        , Merge "S" (makeString "S" s)
                                        , Merge "N" (makeString "N" s)
                                        , Filter (Neg (TestEl "S" v))
                                        , Filter (TestEl "N" v)
                                        , decide s v z ]
                                  , loop vs ]


-- | generating pol_2 -- FIXME rename
genCall:: NetKATM -> Policy
genCall (Mo _ p _ z e _) =  makePolCall p z e

genCallSimple:: NetKATM -> Policy
genCallSimple m = simplify (genCall m)

makePolCall :: [Switch] -> [(Switch, [Port])] -> Internallinks -> Policy
makePolCall [] _ _ = Filter Zero
makePolCall (x:xs) e z = PCup [ callPolSwitch (x, e ! x) x z e
                              , makePolCall xs e z ]

callPolSwitch:: (Switch, [Port]) -> Switch -> Internallinks -> [(Switch, [Port])] -> Policy
callPolSwitch (_, []) _ _ _     = error "where is the home port"
callPolSwitch (_, [_]) _ _ _   = Filter Zero
callPolSwitch (s, _:ps) t z e = PCup [ callPolPort (head ps) s z e
                                     , callPolSwitch (s, ps) t z e ]

callPolPort:: Port -> Switch -> Internallinks -> [(Switch, [Port])] -> Policy
callPolPort p s z = loop where
  loop:: [(Switch, [Port])] -> Policy
  loop [] = Filter Zero
  loop (v:vs) = PCup [ PSeq   [ Add "call" $ ST ("," ++ show s ++ show (destination s p z))
                              , intLink p s z
                              , Merge "S" (makeString "S" (destination s p z))
                              , Merge "N" (makeString "N" (destination s p z))
                              , Mod "ag" (S s)
                              , Mod "pt" (P p)
                              , Merge "S" (makeString "S" s)
                              , Merge "N" (makeString "N" s)
                              , Mod "S" (LS [])
                              , Mod "N" (LS [])
                              , Mod "ag" (S (fst v))
                              , Mod "pt" (P (head (snd v))) ]
                        , loop vs]

-- FIXME drop this?
callString :: Port -> Switch -> Internallinks  -> String
callString p s z = show s ++  " calls " ++ show (destination s p z)

destination :: Switch -> Port -> Internallinks -> Switch
destination s p z | isNothing (lookup (s, p) z) = checksecond s p z
                  | otherwise = fst (z ! (s,p))

checksecond :: Switch -> Port -> Internallinks -> Switch
checksecond _ _ [] = error "pair not found"
checksecond s p (((f,_),(w,t)):zs) | (s,p) == (w,t) = f
                                   | otherwise = checksecond s p zs

intLink :: Port -> Switch -> Internallinks -> Policy
intLink p s z | isNothing (lookup (s, p) z) = check p s z
              | otherwise = PSeq [Filter (Test "ag" (S s))
                                     , Filter (Test "pt" (P p))
                                     , Mod "ag" (S (fst (z ! (s,p))))
                                     , Mod "pt" (P (snd (z ! (s,p)))) ]

check :: Port -> Switch -> Internallinks -> Policy
check _ _ [] = error "pair not found"
check p s (((f,v),(w,t)):zs) | (s,p) == (w,t) = PSeq [ Filter (Test "ag" (S s))
                                                     , Filter (Test "pt" (P p))
                                                     , Mod "ag" (S f)
                                                     , Mod "pt" (P v) ]
                             | otherwise = check p s zs

---- gossip to netkat, creating the packet corresponding to a specific gossip graph

transfer :: GossipGraph -> Packet
transfer [] = []
transfer ((x,(y,z)):xs) = [ ("ag", S x)
                          , ("pt", P (head ports))
                          , ("S", LS [])
                          , ("N", LS [])
                          , (makeString "S" x, LS z)
                          , (makeString "N" x, LS y)
                          , ("call", ST "") ]
      ++ others xs where
        ports = map Port [1..(length gg * length gg)]
        gg = (x,(y,z)):xs

others:: GossipGraph -> [(Field, Value)]
others [] = []
others ((x,(y,z)):xs) = [(makeString "S" x, LS z),
                         (makeString "N" x, LS y)]
                ++ others xs
