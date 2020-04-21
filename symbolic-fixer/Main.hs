{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L
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
      Nothing -> L.putStrLn (showEntryWithEnhancement 1 dummyEntry [])
      Just s -> do
        putStrLn $ "# sent_id = " ++ show sentenceId
        L.putStrLn $ "# text = " <> L.intercalate " " (map entryRaw s)
        let enh = consolidateArcs . enhanceFunction $ s
            ls = zipWith3 showEntryWithEnhancement [1..] s enh
        forM_ ls L.putStrLn
    putStrLn ""

