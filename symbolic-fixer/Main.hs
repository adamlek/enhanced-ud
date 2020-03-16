import System.Environment

import DepLib

main :: IO ()
main = do
  inputs <- getArgs
  ss <- parseManyFiles (parseNivreSentences False) inputs
  mapM_  (mapM_ print) $ take 2 $ ss
