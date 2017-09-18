module GossipKATS where
import Data.List
import Data.Maybe
import Control.Monad

import Data.GraphViz hiding (Star)
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic

-- this part is not used directly, but it might be interesting
toGraph :: NetKATM -> Data.GraphViz.Types.Generalised.DotGraph String
toGraph (Mo hosts switches ports swlinks portlinks hostlinks) =
  graph' $ do
    -- draw all nodes:
    let nodes = map show hosts ++ map show switches ++ map show ports
    mapM_ (\nid -> node nid [toLabel nid]) nodes
    -- draw edges for the three sets: (FIXME _ignored stuff!)
    mapM_ (\(sw,ps) -> mapM_ (\p -> edge (show sw) (show p) []) ps) swlinks
    mapM_ (\((_sw1,p1),(_sw2,p2)) -> edge (show p1) (show p2) []) portlinks
    mapM_ (\(h,p) -> edge (show p) (show h) []) hostlinks

writeToImage :: NetKATM -> IO ()
writeToImage x = do
  resultpath <- runGraphvizCommand Dot (toGraph x) Pdf "themodel.pdf"
  putStrLn $ "A picture of the model was saved as: " ++ resultpath

newtype Host = Host Int deriving (Eq,Ord,Show)
newtype Port = Port Int deriving (Eq,Ord,Show)
newtype Switch = Switch Int deriving (Eq,Ord)

a,b,c :: Switch
a = Switch 0 ; b = Switch 1 ; c = Switch 2

-- defining a netkat model by giving a list of hosts, switches, ports, per switch which ports it has,
-- links between pairs of ports and switches, and for the hosts which switch/ports pairs they are connected to
data NetKATM = Mo
    [Host]
    [Switch]
    [Port]
    [(Switch,[Port])]
    [((Switch,Port),(Switch,Port))]
    [(Host,Port)] deriving (Eq,Show)

type Field = String
data Value = S Switch | LS [Switch] | P Port deriving (Eq,Ord,Show)
type Packet = [(Field,Value)]
type StateVector = [(Switch, State)]
type State = [(Field, [Switch])]
type Element = Switch

-- hardcoded example netkat model
exampleMo1:: NetKATM
exampleMo1= Mo
   [Host 1,Host 2]
   [Switch 1, Switch 2]
   [Port 1,Port 2, Port 3, Port 4]
   [(Switch 1,[Port 1,Port 2]),(Switch 2, [Port 3, Port 4])]
   [((Switch 1, Port 2),(Switch 2, Port 3))]
   [(Host 1, Port 1), (Host 2, Port 4)]

-- another hardcoded example
exampleMo2:: NetKATM
exampleMo2= Mo
   [Host 1,Host 2,Host 3,Host 4]
   [Switch 1, Switch 2]
   [Port 1, Port 2, Port 3, Port 4, Port 5, Port 6]
   [(Switch 1,[Port 1, Port 3, Port 5]),(Switch 2, [Port 2, Port 4, Port 6])]
   [((Switch 1, Port 5),(Switch 2, Port 6))]
   [(Host 1, Port 1), (Host 2, Port 2), (Host 3, Port 3), (Host 4, Port 4)]

-- example packet
examplePacket1 :: Packet
examplePacket1 = [("ag", S a),("pt", P (Port 9)),("S" , LS []), ("N", LS [])]

examplePacket2 :: Packet
examplePacket2 = [("ag", S b),("pt", P (Port 6)),("S" , LS []), ("N", LS [])]

examplePacket3 :: Packet
examplePacket3 = [("ag", S c),("pt", P (Port 3)),("S" , LS []), ("N", LS [])]

examplePacket4 :: Packet
examplePacket4 = [("ag", S a),("pt", P (Port 4)),("S" , LS []), ("N", LS [])]

exampleStateVec1 :: StateVector
exampleStateVec1 = [(a, [("S", [a]),("N", [a,b])]), (b, [("S", [b]),("N", [b,c])]), (c, [("S", [c]),("N", [c])])]

exampleStateVec2 :: StateVector
exampleStateVec2 = [(a, [("S", [a]),("N", [a,b])]), (b, [("S", [b]),("N", [b])])]

example1 :: (Packet,StateVector)
example1 = (examplePacket1, exampleStateVec1)

example2 :: (Packet,StateVector)
example2 = (examplePacket2, exampleStateVec1)

example3 :: (Packet,StateVector)
example3 = (examplePacket3, exampleStateVec1)

example4:: (String,(Packet, StateVector))
example4 = ("",example1)

example5:: (String,(Packet, StateVector))
example5 = ("", example2)

example6:: (String,(Packet, StateVector))
example6 = ("", example3)

example7:: (String,(Packet, StateVector))
example7 = ("", example8)

example8 :: (Packet,StateVector)
example8 = (examplePacket4, exampleStateVec2)

example9:: (String,(Packet, StateVector))
example9 = ("", example1)

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

-- types that are needed. I changed the type of the value to list, because I want to be able to have sets of integers as values of fields, and to be
-- able to test if something is an element of the field's value.



data Predicate = One
               | Zero
               | Test Field Value
               | TestEl Field Element
               | TestAllSecrets Field [Switch] Switch
               | Cup [Predicate]
               | Seq [Predicate]
               | Neg Predicate
               | StateTest Switch State
               deriving (Eq,Ord)

data Policy = Filter Predicate
        | Mod Field Value
        | PCup [Policy]
        | PSeq [Policy]
        | Star Policy
        | StateMod Switch State
        | Merge Field Switch
        | Call Switch Switch
    deriving (Eq, Ord)


evalPred :: [(Packet, StateVector)] -> Predicate -> [(Packet, StateVector)]
evalPred [] _            = []
evalPred (x:xs) One      = x : evalPred xs One
evalPred _      Zero     = []
evalPred ((x,f):xs) (Test g w) | x ! g == w = (x,f) : evalPred xs (Test g w)
                               | otherwise = evalPred xs (Test g w)
evalPred ((x,f):xs) (TestEl g w) | w `elem` whatValue (x ! g) = (x,f) : evalPred xs (TestEl g w)
                                 | otherwise = evalPred xs (TestEl g w)
evalPred ((x,f):xs) (TestAllSecrets g w s) | w == (f ! s) ! g = (x,f) : evalPred xs (TestAllSecrets g w s)
                                      | otherwise = evalPred xs (TestAllSecrets g w s)
evalPred _ (Cup [])      = []
evalPred xs (Cup (p:ps)) = evalPred xs p ++ evalPred xs (Cup ps)
evalPred xs (Seq [])     = evalPred xs One
evalPred xs (Seq (p:ps)) = evalPred (evalPred xs p) (Seq ps)
evalPred (x:xs) (Neg predi) | null (evalPred [x] predi) = x : evalPred xs (Neg predi)
                            | otherwise = evalPred xs (Neg predi)
evalPred ((x,f):xs) (StateTest g w) | f ! g == w = (x,f) : evalPred xs (StateTest g w)
                                    | otherwise = evalPred xs (StateTest g w)

instance Show Predicate where
  show One = "one"
  show Zero = "zero"
  show (Test field value) = field ++ " = " ++ show value
  show (TestEl field element) = show element ++ " ∈ " ++ field
  show (TestAllSecrets field value switch)= show switch ++ "." ++ field ++ " = " ++ show value
  show (Cup xs) = intercalate " + " (map show xs)
  show (Seq xs) = intercalate " • " (map show xs)
  show (Neg x) = "¬" ++ " (" ++ show x ++ ")"
  show (StateTest entry value) = "State(" ++ show entry ++ ") = " ++ show value

-- function for updating a packet while keeping the statevector the same
update :: Field -> Value -> (Packet, StateVector) -> (Packet, StateVector)
update f v (p,s) = ([ (g, if g==f then v else w) | (g,w) <- p ], s)

-- function for updating the statevector
updateVec :: Switch -> State -> (Packet, StateVector) -> (Packet, StateVector)
updateVec f v (p,s) = (p, [ (g, if g==f then v else w) | (g,w) <- s ])

evalPol :: [(Packet, StateVector)] -> Policy -> [(Packet, StateVector)]
evalPol [] _              = []
evalPol xs (Filter predi) = evalPred xs predi
evalPol xs (Mod g w)      = map (update g w) xs
evalPol _ (PCup [])       = []
evalPol xs (PCup (p:ps))  = evalPol xs p ++ evalPol xs (PCup ps)
evalPol xs (PSeq [])      = xs
evalPol xs (PSeq (p:ps))  = evalPol (evalPol xs p) (PSeq ps)
evalPol xs (Star pol)     = lfp extend xs where
  extend :: [(Packet, StateVector)] -> [(Packet, StateVector)]
  extend ps = nub $ ps ++ evalPol ps pol
evalPol xs (StateMod g w) = map (updateVec g w) xs
evalPol xs (Merge f s)    = map (mergeField f s) xs
evalPol xs (Call _ _)     = evalPol xs (Filter One)

instance Show Policy where
  show (Filter predi) = show predi
  show (Mod field value) = field ++ " ← " ++ show value
  show (PCup xs) = intercalate " + " (map show xs)
  show (PSeq xs) = "(" ++ intercalate " • " (map show xs) ++ ")"
  show (Star x) = show x ++ "*"
  show (StateMod entry value) = "State(" ++ show entry ++ ") ← " ++ show value
  show (Merge field switch) = "Merge (" ++ field ++ " , " ++ show switch ++ ")"
  show (Call _ _) = ""

evalPolString :: [(String,(Packet, StateVector))] -> Policy -> [(String,(Packet, StateVector))]
evalPolString [] _                      = []
evalPolString ((s,x):xs) (Filter predi) | null $ evalPred [x] predi = evalPolString xs (Filter predi)
                                        | otherwise = (s, head (evalPred [x] predi)) : evalPolString xs (Filter predi)
evalPolString ((s,x):xs) (Mod g w)      = (s, head (evalPol [x] (Mod g w))) : evalPolString xs (Mod g w)
evalPolString _ (PCup [])               = []
evalPolString xs (PCup (p:ps))          = evalPolString xs p ++ evalPolString xs (PCup ps)
evalPolString xs (PSeq [])              = xs
evalPolString xs (PSeq (p:ps))          = evalPolString (evalPolString xs p) (PSeq ps)
evalPolString xs (Star pol)             = lfp extend xs where
  extend :: [(String,(Packet, StateVector))] -> [(String,(Packet, StateVector))]
  extend ps = nub $ ps ++ evalPolString ps pol
evalPolString ((s,x):xs) (StateMod g w) = (s, updateVec g w x) : evalPolString xs (StateMod g w)
evalPolString ((s,x):xs) (Merge f w)    = (s, mergeField f w x) : evalPolString xs (Merge f w)
evalPolString ((s,x):xs) (Call z w)     = (s ++ show z ++ " calls " ++ show w ++ ", ", x) : evalPolString xs (Call z w)

-- | Merge the lists of field f in the packet and switch i in the StateVector.
mergeField:: Field -> Switch -> (Packet, StateVector) -> (Packet, StateVector)
mergeField f i (p, s) = (newPacket, newSV) where
  newPacket = [ (g,  if g==f  then LS (sort $ nub $ ((s ! i) ! f) ++ whatValue w) else w) | (g,w) <- p ]
  newSV     = [ (sw, if sw==i then newST else pr) | (sw, pr) <- s ]
  newST = [ (g,  if g==f  then sort $ nub $ whatValue (p ! f) ++ w else w) | (g,w) <- s ! i ]

whatValue :: Value -> [Switch]
whatValue (LS w)  = w
whatValue (P _)   = error "not the right type"
whatValue (S _)   = error "not the right type"

lfp :: Eq a => (a -> a) -> a -> a
lfp f x | x == f x  = x
        | otherwise = lfp f (f x)

--function that generates the possible states:
states:: [Switch] -> [State]
states [] = error "No agents"
states (x:xs) = nub (statesFin (x:xs) (x:xs)) where
  statesFin _ [] = error "No agents"
  statesFin [] _ = []
  statesFin (z:zs) y = genStates z y ++ statesFin zs y

genStates:: Switch -> [Switch] -> [State]
genStates _ [] = error "No list"
genStates x y = loop (subsetsEl x y) where
  loop = foldr (\z -> (++) (genState z y)) []

genState:: [Switch] -> [Switch] -> [State]
genState x y = loop (subsetOfSets x y) where
  loop:: [[Switch]] -> [State]
  loop = map (\z -> [("S", x), ("N", z)])

subsets:: (Eq a) => [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

subsetsEl:: (Eq a) => a -> [a] -> [[a]]
subsetsEl el set = filter (elem el) (subsets set)

subsetOf:: (Eq a) => [a] -> [a] -> Bool
subsetOf _ []      = False
subsetOf [] _      = True
subsetOf (x:xs)  y | x `elem` y = subsetOf xs y
                   | otherwise = False

subsetOfSets:: (Eq a) => [a] -> [a] -> [[a]]
subsetOfSets [] x = subsets x
subsetOfSets _ [] = []
subsetOfSets x y  = filter (subsetOf x) (subsets y)

callSequence :: NetKATM -> [(Packet, StateVector)] -> [(Packet, StateVector)]
callSequence mo packstate = evalPol packstate (PSeq [polDistributeSimple mo,
                                                Star (PSeq [genModelSimple mo, genCallSimple mo])])

polDistribute :: NetKATM -> Policy
polDistribute (Mo _ _ _ ((_, []):_) _ _) = error "no ports"
polDistribute (Mo _ _ _ [] _ _) = Filter Zero
polDistribute (Mo h s k ((w,p:_):zs) e f) = PCup [PSeq [Mod "ag" (S w),
                                                        Mod "pt" (P p)],
                                                  polDistribute (Mo h s k zs e f)]

polDistributeSimple:: NetKATM -> Policy
polDistributeSimple m = simplify (polDistribute m)


callSequenceString :: NetKATM -> [(String,(Packet, StateVector))] -> [(String,(Packet, StateVector))]
callSequenceString mo stringpackstate = evalPolString stringpackstate
                                        (PSeq [polDistributeSimple mo,
                                               Star (PSeq [genModelSimple mo, genCallSimple mo])])

callSequenceStringFormat :: NetKATM -> [(String,(Packet, StateVector))] -> IO ()
callSequenceStringFormat mo stringpackstate = mapM_ print $ evalPolString stringpackstate (Star (PSeq [genModelSimple mo, genCallSimple mo]))

niceOutput:: NetKATM -> [(String,(Packet, StateVector))] -> [(String,(Packet, StateVector))]
niceOutput mo strpacst = filter (\z -> isUnique (fst z) (map fst $ callSequenceString mo strpacst)) (callSequenceString mo strpacst)

filteredOutput:: NetKATM -> [(String,(Packet, StateVector))] -> String
filteredOutput mo strpacst =
  "These are successful call sequences:\n  "
  ++ intercalate "\n  " successes
  ++ "\nThese are not successful call sequences:\n  "
  ++ intercalate "\n" failures where
    failures = nub $ failString mo outputs
    outputs = niceOutput mo strpacst
    successes = nub $ successString mo outputs

showOutput:: NetKATM -> [(String,(Packet, StateVector))] -> IO ()
showOutput mo strpacst = putStrLn $ filteredOutput mo strpacst

failString:: NetKATM -> [(String,(Packet, StateVector))] -> [String]
failString Mo{} [] = []
failString (Mo h s p z e f) (x:xs) | null $ evalPol [snd x] (successPol s s (Mo h s p z e f  ))
                                        =  fst x : failString (Mo h s p z e f) xs
                                   | otherwise = failString (Mo h s p z e f) xs

successString:: NetKATM -> [(String,(Packet, StateVector))] -> [String]
successString Mo{} [] = []
successString (Mo h s p z e f) (x:xs) | null $ evalPol [snd x] (successPol s s (Mo h s p z e f  ))
                                          = successString (Mo h s p z e f) xs
                                      | otherwise = fst x : successString (Mo h s p z e f) xs

isUnique:: String -> [String] -> Bool
isUnique _ []      = True
isUnique x (v:vs) | myPrefix x v = False
                  | otherwise    = isUnique x vs

myPrefix :: String -> String -> Bool
myPrefix [] [] = False
myPrefix [] _  = True
myPrefix _ []  = False
myPrefix (x:xs) (y:ys) = (x == y) && myPrefix xs ys

successPol:: [Switch] -> [Switch] -> NetKATM -> Policy
successPol [] _ _ = Filter One
successPol (s:ws) x mo = PSeq [ Filter (TestAllSecrets "S" x s)
                              , successPol ws x mo ]

-- generating pol_1
genModel:: NetKATM -> Policy
genModel (Mo _ p _ d e _) = genForward p d e

genModelSimple:: NetKATM -> Policy
genModelSimple m = simplify (genModel m)

genForward:: [Switch] -> [(Switch, [Port])] -> Internallinks -> Policy
genForward _ _ [] = error "no links"
genForward _ [] _ = Filter Zero
genForward [] _ _ = error "no switches"
genForward t ((s,p):xs) z = PCup [genForwardSwitch t (s,p) z, genForward t xs z]

(!) :: Eq a => [(a,b)] -> a -> b
(!) rel x = fromJust $ lookup x rel

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
                                        , Merge "S" s
                                        , Merge "N" s
                                        , Filter (Neg (TestEl "S" v))
                                        , Filter (TestEl "N" v)
                                        , decide s v z ]
                                  , loop vs ]
-- | generating pol_2
genCall:: NetKATM -> Policy
genCall (Mo _ p _ d e _) =  makePolCall p d e

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
                              , Merge "S" (destination s p z)
                              , Merge "N" (destination s p z)
                              , Mod "ag" (S s)
                              , Mod "pt" (P p)
                              , Merge "S" s
                              , Merge "N" s
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
simStep (StateMod s p)     = StateMod s p
simStep (Merge f s)        = Merge f s
simStep (Call s w)         = Call s w

-- | Indent with three spaces
indent :: String
indent = "   "

-- | Verbosely evaluate a Policy
verboseEvalPol :: String -> [(Packet, StateVector)] -> Policy -> IO [(Packet, StateVector)]
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
verboseEvalPol prefix xs (StateMod g w) = do
  putStrLn $ prefix ++ "Modified the state, result:  " ++ show (map (updateVec g w) xs)
  return $ map (updateVec g w) xs
verboseEvalPol prefix xs (Merge f s) = do
  putStr $ prefix ++ "Merging field " ++ f ++ " with " ++ show s ++ " ... "
  let newxs = map (mergeField f s) xs
  putStrLn $ "new " ++ f ++ " values in different tuples: " ++ show [ fst psv ! f | psv <- newxs ]
  return $ map (mergeField f s) xs
verboseEvalPol prefix xs (Call s w) = do
  putStrLn $ prefix ++ show s ++ " calls " ++ show w
  return $ evalPol xs (Filter One)

-- | Least fixpoint of an IO function
lfpIO :: Eq a => (a -> IO a) -> a -> IO a
lfpIO f x = do
  y <- f x
  if x == y
      then return y
      else lfpIO f y

-- | Translate a model to a policy, then verbosely evaluate it on the given packet states.
verboseCallSequence :: NetKATM -> [(Packet, StateVector)] -> IO ()
verboseCallSequence mo packstate = do
  results <- verboseEvalPol "" packstate (Star (PSeq [genModelSimple mo, genCallSimple mo]))
  mapM_ print results


--appendix
instance Show Switch where
    show (Switch 0) = "a";
    show (Switch 1) = "b";
    show (Switch 2) = "c";
    show (Switch n) = "Switch " ++ show n

showStatesIntermediate :: [Switch] -> String
showStatesIntermediate z = loop 1 (states z) where
  loop:: Int -> [State] -> String
  loop _ []     = []
  loop n (x:xs) = "State " ++ show n ++ ": " ++ show x ++ "\n" ++ loop (n+1) xs

showStates :: [Switch] -> IO()
showStates z = putStrLn (showStatesIntermediate z)

tex :: String -> String
tex = concatMap replace where
  replace x = fromMaybe [x] (lookup x texPairs)
  texPairs =
    [ ('•', " \\cdot ")
    , ('¬', " \\lnot ")
    , ('*', " \\ast ")
    , ('←', " \\leftarrow ")
    ]

showStatesLatex:: [Switch]->IO()
showStatesLatex z = putStrLn (tex (showStatesIntermediate z))

---from gossip graph to initial packets
type Item = (Switch,([Switch],[Switch]))
type GossipGraph = [Item]

transfer:: GossipGraph -> [(Packet,StateVector)]
transfer gg = loop gg ports where
  ports = makeList (length gg * length gg)
  loop [] _                  = []
  loop _ []                  = []
  loop ((x,(_,_)):xs) (k:ks) = ([("ag", S x),
                                ("pt", P k),
                                ("S", LS []),
                                ("N", LS [])],
                                statevector gg)
        : loop xs (deleteList (length gg) (k:ks))

statevector:: GossipGraph -> StateVector
statevector [] = []
statevector ((x,(y,z)):xs) = (x, [("S", z),("N", y)])
                                      : statevector xs

deleteList:: Eq a => Int -> [a] -> [a]
deleteList _ []   = error "integer too big"
deleteList 0 ys   = ys
deleteList n (_:ys) = deleteList (n-1) ys

withString:: [(Packet,StateVector)] -> [(String, (Packet, StateVector))]
withString = map (\ x -> ("", x))

gossipToNetkat:: GossipGraph -> (String, (Packet, StateVector))
gossipToNetkat gg = ("", transferSimple gg)

transferSimple:: GossipGraph -> (Packet, StateVector)
transferSimple gg = ([("ag", S a),
                     ("pt", P (Port 1)),
                     ("S", LS []),
                     ("N", LS [])],
                    statevector gg)

gossipExample:: GossipGraph
gossipExample = [ (a,([a,b],[a]))
                , (b,([b,c],[b]))
                , (c,([c],[c])) ]

gossipExample2:: GossipGraph
gossipExample2 = [ (a,([a,b],[a]))
                 , (b,([b,c],[b]))
                 , (c,([a,c],[c])) ]

gossipExample3:: GossipGraph
gossipExample3 = [ (a,([a,c],[a]))
                 , (b,([b,c],[b]))
                 , (c,([c],[c])) ]
