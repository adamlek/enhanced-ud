import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad
import DepLib
import Fixer

main :: IO ()
main = do
  inputs <- getArgs
  ss <- parseManyFiles (parseNivreSentences False) inputs
  forM_ ss $ \s -> do
    let enh = consolidateArcs . allEnhancer $ s
        ls = zipWith3 showEntryWithEnhancement [1..] s enh
    putStrLn ""
    forM_ ls L.putStrLn


