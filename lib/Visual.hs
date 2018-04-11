module Visual where

import Data.GraphViz
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Datatypes

-- this part is not used directly, but it might be interesting
toGraph :: NetKATM -> Data.GraphViz.Types.Generalised.DotGraph String
toGraph (Mo hosts switches ports swlinks portlinks hostlinks) =
  graph' $ do
    -- draw all nodes:
    let nodes = map show hosts ++ map show switches ++ map show ports
    mapM_ (\nid -> node nid [toLabel nid]) nodes
    -- draw edges for the three sets: (NOTE _ignore port-owner switch!)
    mapM_ (\(sw,ps) -> mapM_ (\p -> edge (show sw) (show p) []) ps) swlinks
    mapM_ (\((_sw1,p1),(_sw2,p2)) -> edge (show p1) (show p2) []) portlinks
    mapM_ (\(h,p) -> edge (show p) (show h) []) hostlinks

writeToImage :: NetKATM -> IO ()
writeToImage x = do
  resultpath <- runGraphvizCommand Dot (toGraph x) Pdf "themodel.pdf"
  putStrLn $ "A picture of the model was saved as: " ++ resultpath

showImage :: NetKATM -> IO ()
showImage x = runGraphvizCanvas' (toGraph x) Xlib
