module NetKAT where

import Data.List
import Control.Monad
import Datatypes
import Helperfunctions

-- | Evaluate a predicate
evalPred :: [Packet] -> Predicate -> [Packet]
evalPred [] _            = []
evalPred xs One          = xs
evalPred _  Zero         = []
evalPred xs (Test g w)   = [ x | x <- xs, x ! g == w ]
evalPred xs (TestEl g w) = [ x | x <- xs, w `elem` whatValue (x ! g) ]
evalPred _  (Cup [])     = []
evalPred xs (Cup (p:ps)) = evalPred xs p ++ evalPred xs (Cup ps)
evalPred xs (Seq [])     = evalPred xs One
evalPred xs (Seq (p:ps)) = evalPred (evalPred xs p) (Seq ps)
evalPred xs (Neg predi)  = [ x | x <- xs, null (evalPred [x] predi) ] -- is this the same?

-- | Evaluate a policy
evalPol :: [Packet] -> Policy -> [Packet]
evalPol [] _              = []
evalPol xs (Filter predi) = evalPred xs predi
evalPol xs (Mod g w)      = map (update g w) xs
evalPol xs (Add g w)      = map (addValue g w) xs
evalPol _  (PCup [])      = []
evalPol xs (PCup (p:ps))  = evalPol xs p ++ evalPol xs (PCup ps)
evalPol xs (PSeq [])      = xs
evalPol xs (PSeq (p:ps))  = evalPol (evalPol xs p) (PSeq ps)
evalPol xs (Star pol)     = lfp extend xs where
  extend :: [Packet] -> [Packet]
  extend ps = nub $ ps ++ evalPol ps pol
evalPol xs (Merge f s)    = map (mergeField s f . mergeField f s) xs

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
verboseEvalPol prefix xs (Add g w) = do
    putStrLn $ prefix ++ "Adding " ++ g ++ " += " ++ show w
    return $ map (addValue g w) xs
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

-- | Generate a specific NetKAT model for n hosts and switches
netkatmFor :: Int -> NetKATM
netkatmFor n = netkatmGen (map Host [1..n]) (map Switch [0..(n-1)])

-- | Generate a NetKAT model
-- Each switch is connected to one host and vice versa.
-- Between the switches we have a total graph.
netkatmGen :: [Host] -> [Switch] -> NetKATM
netkatmGen h s
  | null h               = error "no hosts"
  | null s               = error "no switches"
  | length h /= length s = error "wrong number"
  | otherwise = Mo hosts switches ports combiswitchport internallinks outerlinks where
      hosts           = h
      switches        = s
      ports           = map Port [1..(length h * length s)]
      combiswitchport = combineSwitchesPorts s ports
      internallinks   = internalLinks combiswitchport
      outerlinks      = combineHosts h combiswitchport

      combineSwitchesPorts :: [Switch] -> [Port] -> [(Switch,[Port])]
      combineSwitchesPorts [] _  = []
      combineSwitchesPorts _ []  = []
      combineSwitchesPorts (w:ws) ps = loop (w:ws) (w:ws) ps where
        loop:: [Switch] -> [Switch] -> [Port] -> [(Switch,[Port])]
        loop _  _ [] = []
        loop [] _  _ = []
        loop (y:ys) xs ks = (y, port) : loop ys xs (ks\\port) where
          port = take (length xs) ks

      combineHosts:: [Host] -> [(Switch,[Port])] -> [(Host,Port)]
      combineHosts _ []  = []
      combineHosts [] _  = []
      combineHosts _ ((_,[]):_) = []
      combineHosts (h0:hs) ((_, p:_):zs) = (h0,p) : combineHosts hs zs

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
