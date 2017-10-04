module Examples where
import Datatypes

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

-- example packet when working with 3 agents
examplePacket1 :: Packet
examplePacket1 = [("ag", S a),("pt", P (Port 9)),("S" , LS []), ("N", LS []),
                  ("Sa", LS [a]), ("Sb", LS [b]), ("Sc", LS [c]),
                  ("Na", LS [a,b]), ("Nb", LS [b,c]), ("Nc", LS [c]),
                  ("call", ST "")]

-- example packet when working with 2 agents
examplePacket2 :: Packet
examplePacket2 = [("ag", S a),("pt", P (Port 4)),("S" , LS []), ("N", LS []),
                  ("Sa", LS [a]), ("Sb", LS [b]),
                  ("Na", LS [a,b]), ("Nb", LS [b]),
                  ("call", ST "")]

-- example packet when working with 4 agents
examplemoreagents:: Packet
examplemoreagents = [("ag", S a),("pt", P (Port 16)),("S" , LS []), ("N", LS []),
                  ("Sa", LS [a]), ("Sb", LS [b]), ("Sc", LS [c]), ("Sd", LS [d]),
                  ("Na", LS [a,b]), ("Nb", LS [b,c]), ("Nc", LS [c,d]), ("Nd", LS [b,d]),
                  ("call", ST "")]


-- examples of gossip graphs with three agents
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
