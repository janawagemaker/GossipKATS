module GossipFrenetic where

import Datatypes
import GossipNetKAT
import Helperfunctions

import System.IO
import System.Process

data Question = StrongSuccess | WeakSuccess

ggQuestionToNetKat :: GossipGraph -> Question -> String
ggQuestionToNetKat gg q = removeCalls $ show lhs ++ " == " ++ show rhs where
  ws = map fst gg
  (lhs,rhs) = case q of
    -- equivalence holds  iff  strongly succesful:
    StrongSuccess -> ( PSeq [ thisGraphPol gg, lnsPol ws, stopPol ws ]
                     , PSeq [ thisGraphPol gg, lnsPol ws, successPol ws ] )
    -- equivalence is FALSE  iff  weakly succesful:
    WeakSuccess   -> ( PSeq [ thisGraphPol gg, lnsPol ws, successPol ws ]
                     , Filter Zero )
  removeCalls :: String -> String
  removeCalls = flip withoutMult $
    "call=[];" : [";call ←+ [(" ++ show i ++ "," ++ show j ++ ")]" | (i,j) <- calls ]
  calls = [ (i,j) | i <- ws, j <- ws ]

frenetic :: String
frenetic = "dist/frenetic/frenetic.native" -- adapt path to frenetic here

ggAskFrenetic :: GossipGraph -> Question -> IO Bool
ggAskFrenetic gg q = do
  let str = ggQuestionToNetKat gg q
  putStrLn str
  writeFile "policy.temp" str
  let command = frenetic ++ " decide " ++ "policy.temp"
  putStrLn $ "Running: " ++ command
  (_inp,out,err,pid) <- runInteractiveCommand command
  _ <- waitForProcess pid
  hGetContents err >>= putStrLn
  result <- hGetContents out
  putStrLn result
  return $ result == "true\n"
