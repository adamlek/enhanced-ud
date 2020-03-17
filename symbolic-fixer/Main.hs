import System.Environment

import DepLib
import Fixer

main :: IO ()
main = do
  inputs <- getArgs
  ss <- parseManyFiles (parseNivreSentences False) inputs
  let arcs = map (consolidateArcs . allEnhancer) ss
  mapM_  (mapM_ print) $  arcs


