{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import DepLib
import Fixer


dummyEntry :: Entry
dummyEntry = Entry {entryRaw = d, entryLemma=d, entryPos = d, entryXPos = d, entryMisc = "", entryFeatures=[], entryParent=0, entryLabel = "ROOT"}
 where d = "dummy"

main :: IO ()
main = do
  inputs' <- getArgs
  let (enhanceFunction,inputs) =
        case inputs' of
          ("--dummy":rest) -> (noEnhancer,rest)
          _ -> (allEnhancer,inputs')
  sentences <- parseManyFiles (parseNivreSentences False) inputs
  forM_ (zip [(1::Int)..] sentences) $ \(sentenceId,sentence) -> do
    case sentence of
      Nothing -> T.putStrLn (showEntryWithEnhancement 1 dummyEntry [])
      Just s -> do
        putStrLn $ "# sent_id = " ++ show sentenceId
        T.putStrLn $ "# text = " <> T.intercalate " " (map entryRaw s)
        let enh = consolidateArcs . enhanceFunction $ s
            ls = zipWith3 showEntryWithEnhancement [1..] s enh
        forM_ ls T.putStrLn
    putStrLn ""

