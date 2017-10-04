module GossipKATS where
import Data.List
import Data.Maybe
import Control.Monad
import Examples
import Datatypes
import Extra
import Helperfunctions

-- netkat model generator for gossip
netkatmGen :: [Host] -> [Switch] -> NetKATM
netkatmGen [] _ = error "no hosts"
netkatmGen _ [] = error "no switches"
netkatmGen h s | length h /= length s = error "wrong number"
               | otherwise = netGosGen h s where
   netGosGen x k = Mo hosts switches ports combiswitchport internallinks outerlinks where
            hosts           = x
            switches        = k
            ports           = makeList (portCount x k)
            combiswitchport = combineSwitchesPorts s ports
            internallinks   = internalLinks combiswitchport
            outerlinks      = combineHosts h combiswitchport

combineSwitchesPorts :: [Switch] -> [Port] -> [(Switch,[Port])]
combineSwitchesPorts [] [] = []
combineSwitchesPorts [] _  = []
combineSwitchesPorts _ []  = []
combineSwitchesPorts (w:ws) ps = loop (w:ws) (w:ws) ps where
  loop:: [Switch] -> [Switch] -> [Port] -> [(Switch,[Port])]
  loop [] _ [] = []
  loop _  _ [] = []
  loop [] _  _ = []
  loop (y:ys) xs ks = (y, port) : loop ys xs (ks\\port) where
    port = take (length xs) ks

portCount :: [Host] -> [Switch] -> Int
portCount h s = length h * length s

makeList :: Int -> [Port]
makeList 0 = []
makeList n = Port n : makeList (n-1)

combineHosts:: [Host] -> [(Switch,[Port])] -> [(Host,Port)]
combineHosts _ []  = []
combineHosts [] _  = []
combineHosts _ ((_,[]):_) = []
combineHosts (h:hs) ((_, p:_):zs) = (h, p):combineHosts hs zs

internalLinksPerSwitch :: [(Switch,[Port])] -> [((Switch,Port),(Switch,Port))]
internalLinksPerSwitch [] = []
internalLinksPerSwitch ((_,[]):_) = []
internalLinksPerSwitch [(_,_)] = []
internalLinksPerSwitch ((t, _ : ps):(w, fs):zs) = ((t,head ps), (w, head (tail fs))): internalLinksPerSwitch ((t,ps):zs)

internalLinks :: [(Switch,[Port])] -> [((Switch,Port),(Switch,Port))]
internalLinks [] = []
internalLinks (z:zs) = internalLinksPerSwitch (z:zs) ++ internalLinks (eliminate zs) where
  eliminate:: [(Switch,[Port])] -> [(Switch,[Port])]
  eliminate = map (\(x,_:ys) -> (x, ys))


--- evaluating policies and predicates

evalPred :: [Packet] -> Predicate -> [Packet]
evalPred [] _            = []
evalPred (x:xs) One      = x : evalPred xs One
evalPred _      Zero     = []
evalPred (x:xs) (Test g w) | x ! g == w = x : evalPred xs (Test g w)
                           | otherwise = evalPred xs (Test g w)
evalPred (x:xs) (TestEl g w) | w `elem` whatValue (x ! g) = x : evalPred xs (TestEl g w)
                             | otherwise = evalPred xs (TestEl g w)
evalPred _ (Cup [])      = []
evalPred xs (Cup (p:ps)) = evalPred xs p ++ evalPred xs (Cup ps)
evalPred xs (Seq [])     = evalPred xs One
evalPred xs (Seq (p:ps)) = evalPred (evalPred xs p) (Seq ps)
evalPred (x:xs) (Neg predi) | null (evalPred [x] predi) = x : evalPred xs (Neg predi)
                            | otherwise = evalPred xs (Neg predi)

evalPol :: [Packet] -> Policy -> [Packet]
evalPol [] _              = []
evalPol xs (Filter predi) = evalPred xs predi
evalPol xs (Mod g w)      = map (update g w) xs
evalPol _ (PCup [])       = []
evalPol xs (PCup (p:ps))  = evalPol xs p ++ evalPol xs (PCup ps)
evalPol xs (PSeq [])      = xs
evalPol xs (PSeq (p:ps))  = evalPol (evalPol xs p) (PSeq ps)
evalPol xs (Star pol)     = lfp extend xs where
  extend :: [Packet] -> [Packet]
  extend ps = nub $ ps ++ evalPol ps pol
evalPol xs (Merge f s)    = map (mergeField s f . mergeField f s) xs
evalPol (x:xs) (Call z y) = evalPol [x]
                           (Mod "call" (ST (whatValueString (x ! "call") ++ ", " ++ show z ++ show y)))
                            ++ evalPol xs (Call z y)

-- | Verbosely evaluate a Policy
verboseEvalPol :: String -> [Packet] -> Policy -> IO [Packet]
verboseEvalPol prefix [] _ = do
    putStrLn $ prefix ++ "no packets left"
    return []
verboseEvalPol prefix xs (Filter predi) = do
    let result = evalPred xs predi
    putStr $ prefix ++ "filtering with predicate " ++ show predi ++ " ... "
    putStrLn $ show (length result) ++ "/" ++ show (length xs) ++ " packets survived."
    return result
verboseEvalPol prefix xs (Mod g w) = do
    putStrLn $ prefix ++ "Modifying " ++ g ++ " := " ++ show w
    return $ map (update g w) xs
verboseEvalPol prefix _  (PCup []) = do
    putStrLn $ prefix ++ "empty PCup, FAIL!"; return []
verboseEvalPol prefix xs (PCup (p:ps)) = do
    putStrLn $ prefix ++ "PCup with " ++ show (length (p:ps)) ++ " elements"
    x <- concat <$> mapM (verboseEvalPol (prefix++"U"++indent) xs) (p:ps)
    putStrLn prefix
    return x
verboseEvalPol prefix xs (PSeq []) = do
    putStrLn $ prefix ++ "empty seq, SKIP!"; return xs
verboseEvalPol prefix xs (PSeq (p:ps)) = do
    putStrLn $ prefix ++ "PSeq of length " ++ show (length (p:ps))
    foldM (verboseEvalPol (prefix++";"++indent)) xs (p:ps) -- folding monad voodoo
verboseEvalPol prefix xs (Star pol) = do
    putStrLn "Oh my, I see a star!"
    lfpIO extend xs where
      extend ps = do
        putStrLn $ prefix ++ "We have " ++ show (length ps) ++ " packets now, extending to see if this is the fixpoint ..."
        ys <- verboseEvalPol (prefix++"*"++indent) ps pol
        let newps = nub $ ps ++ ys
        putStr $ prefix ++ "We have " ++ show (length newps) ++ " packets now."
        when (ps /= newps) $ putStrLn $ " The " ++ show (length (newps \\ ps)) ++ " new ones are these:"
        when (ps /= newps) $ mapM_ print (newps \\ ps)
        when (ps == newps) $ putStrLn " So we found the fixpoint!"
        return newps
verboseEvalPol prefix xs (Merge f s) = do
    putStr $ prefix ++ "Merging field " ++ f ++ " with " ++ show s ++ " ... "
    let newxs = map (mergeField s f . mergeField f s) xs
    putStrLn $ "new " ++ f ++ " and " ++ s ++ " values in different tuples: " ++ show [ psv ! f | psv <- newxs ]
    return $ map (mergeField s f . mergeField f s) xs
--verboseEvalPol prefix xs (Call s w) = do
--    putStrLn $ prefix ++ show s ++ " calls " ++ show w
--    return $ evalPol xs (Filter One)


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
niceOutput mo pacst = filter (\z -> isUnique (whatValueString (z ! "call")) (makeCallList $ callSequence mo pacst)) (callSequence mo pacst)

makeCallList:: [Packet] -> [String]
makeCallList = map (\ x -> whatValueString (x ! "call"))

failString:: NetKATM -> [Packet] -> [String]
failString Mo{} [] = []
failString (Mo h s p z e f) (x:xs) | null $ evalPol [x] (successPol s s (Mo h s p z e f  ))
                                        =  whatValueString (x ! "call") : failString (Mo h s p z e f) xs
                                   | otherwise = failString (Mo h s p z e f) xs

successString:: NetKATM -> [Packet] -> [String]
successString Mo{} [] = []
successString (Mo h s p z e f) (x:xs) | null $ evalPol [x] (successPol s s (Mo h s p z e f  ))
                                          = successString (Mo h s p z e f) xs
                                      | otherwise = whatValueString (x ! "call") : successString (Mo h s p z e f) xs

-- A function generating a policy testing whether LNS was successful
successPol:: [Switch] -> [Switch] -> NetKATM -> Policy
successPol [] _ _ = Filter One
successPol (s:ws) x mo = PSeq [ Filter (Test (makeString "S" s) (LS x))
                              , successPol ws x mo ]

-- generating pol_1
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


-- | generating pol_2
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
  loop (v:vs) = PCup [ PSeq   [ Call s (destination s p z),
                                intLink p s z
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

callString:: Port -> Switch -> Internallinks  -> String
callString p s z = show s ++  " calls " ++ show (destination s p z)

destination:: Switch -> Port -> Internallinks -> Switch
destination s p z | isNothing (lookup (s, p) z) = checksecond s p z
                  | otherwise = fst (z ! (s,p))

checksecond:: Switch -> Port -> Internallinks -> Switch
checksecond _ _ [] = error "pair not found"
checksecond s p (((f,_),(w,t)):zs) | (s,p) == (w,t) = f
                                   | otherwise = checksecond s p zs

intLink:: Port -> Switch -> Internallinks -> Policy
intLink p s z | isNothing (lookup (s, p) z) = check p s z
              | otherwise = PSeq [Filter (Test "ag" (S s))
                                     , Filter (Test "pt" (P p))
                                     , Mod "ag" (S (fst (z ! (s,p))))
                                     , Mod "pt" (P (snd (z ! (s,p)))) ]

check:: Port -> Switch -> Internallinks -> Policy
check _ _ [] = error "pair not found"
check p s (((f,v),(w,t)):zs) | (s,p) == (w,t) = PSeq [Filter (Test "ag" (S s))
                                                    , Filter (Test "pt" (P p))
                                                    , Mod "ag" (S f)
                                                    , Mod "pt" (P v) ]
                             | otherwise = check p s zs

---- gossip to netkat, creating the packet corresponding to a specific gossip graph

transfer:: GossipGraph -> Packet
transfer [] = []
transfer ((x,(y,z)):xs) = [("ag", S x),
                              ("pt", P (head ports)),
                              ("S", LS []),
                              ("N", LS []),
                              (makeString "S" x, LS z),
                              (makeString "N" x, LS y),
                              ("call", ST "")]
      ++ others xs where
        ports = makeList (length gg * length gg)
        gg = (x,(y,z)):xs

others:: GossipGraph -> [(Field, Value)]
others [] = []
others ((x,(y,z)):xs) = [(makeString "S" x, LS z),
                         (makeString "N" x, LS y)]
                ++ others xs
