module Datatypes where

import Data.List

-- types and constants for NetKAT

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
data Value = I Int | S Switch | LS [Switch] | P Port | ST String | LC Sequence deriving (Eq,Ord,Show)
type Packet = [(Field,Value)]
type Element = Switch

instance Show Switch where
    show (Switch 0) = "a";
    show (Switch 1) = "b";
    show (Switch 2) = "c";
    show (Switch 3) = "d";
    show (Switch n) = "Switch " ++ show n

data Predicate = One
                 | Zero
                 | Test Field Value
                 | Cup [Predicate]
                 | Seq [Predicate]
                 | Neg Predicate
                 deriving (Eq,Ord)

data Policy = Filter Predicate
              | Mod Field Value
              | Add Field Value
              | PCup [Policy]
              | PSeq [Policy]
              | Star Policy
              deriving (Eq, Ord)

-- If ... then ... (else do nothing) for binary NetKAT fields
ifthen :: Field -> Policy -> Policy
ifthen field thenPol = PCup [ PSeq [ Filter (Test field (I 1)), thenPol ]
                            , Filter (Test field (I 0)) ]

ppValue :: Value -> String
ppValue (I n)          = show n
ppValue (S (Switch n)) = show n
ppValue (LS ls)        = show ls
ppValue (P (Port n))   = show n
ppValue (ST s)         = s
ppValue (LC calls)     = show calls

instance Show Predicate where
  show One = "id"
  show Zero = "drop"
  show (Test field value) = field ++ "=" ++ ppValue value
  show (Cup xs) = "(" ++ intercalate "+" (map show xs) ++ ")"
  show (Seq xs) = "(" ++ intercalate ";" (map show xs) ++ ")"
  show (Neg x) = "¬" ++ "(" ++ show x ++ ")"

instance Show Policy where
  show (Filter predi) = show predi
  show (Mod field value) = field ++ ":=" ++ ppValue value
  show (Add field value) = field ++ " ←+ " ++ ppValue value
  show (PCup xs) = "(" ++ intercalate "+" (map show xs) ++ ")"
  show (PSeq xs) = "(" ++ intercalate ";" (map show xs) ++ ")"
  show (Star x) = show x ++ "*"

-- types for dynamic gossip

type Agent = Switch
type GossipGraph = [Item]
type Item = (Agent,([Agent],[Agent])) -- (agent,(N-Relation,S-Relation))
type Call = (Agent,Agent)
type Sequence = [Call]
