module Datatypes where
import Data.List

newtype Host = Host Int deriving (Eq,Ord,Show)
newtype Port = Port Int deriving (Eq,Ord,Show)
newtype Switch = Switch Int deriving (Eq,Ord)

a,b,c,d :: Switch
a = Switch 0 ; b = Switch 1 ; c = Switch 2 ; d = Switch 3 ;

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
data Value = S Switch | LS [Switch] | P Port | ST String deriving (Eq,Ord,Show)
type Packet = [(Field,Value)]
type Element = Switch
type Item = (Switch,([Switch],[Switch]))
type GossipGraph = [Item]

instance Show Switch where
    show (Switch 0) = "a";
    show (Switch 1) = "b";
    show (Switch 2) = "c";
    show (Switch 3) = "d";
    show (Switch n) = "Switch " ++ show n

data Predicate = One
                 | Zero
                 | Test Field Value
                 | TestEl Field Element
                 | Cup [Predicate]
                 | Seq [Predicate]
                 | Neg Predicate
                 deriving (Eq,Ord)

data Policy = Filter Predicate
              | Mod Field Value
              | PCup [Policy]
              | PSeq [Policy]
              | Star Policy
              | Merge Field Field
              | Call Switch Switch
              deriving (Eq, Ord)

instance Show Predicate where
    show One = "one"
    show Zero = "zero"
    show (Test field value) = field ++ " = " ++ show value
    show (TestEl field element) = show element ++ " ∈ " ++ field
    show (Cup xs) = intercalate " + " (map show xs)
    show (Seq xs) = intercalate " • " (map show xs)
    show (Neg x) = "¬" ++ " (" ++ show x ++ ")"

instance Show Policy where
   show (Filter predi) = show predi
   show (Mod field value) = field ++ " ← " ++ show value
   show (PCup xs) = intercalate " + " (map show xs)
   show (PSeq xs) = "(" ++ intercalate " • " (map show xs) ++ ")"
   show (Star x) = show x ++ "*"
   show (Merge field switch) = "Merge (" ++ field ++ " , " ++ show switch ++ ")"
   show (Call _ _) = ""
