import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad
import DepLib
import Fixer

main :: IO ()
main = do
  inputs <- getArgs
  ss <- parseManyFiles (parseNivreSentences False) inputs
  let enhancementss = map (map showEnhancement . consolidateArcs . allEnhancer) ss
  forM_ enhancementss $ \es -> do
    putStrLn "-----"
    forM_ es $ \e -> do
       L.putStrLn e
  -- mapM_ (mapM_ print) arcs


