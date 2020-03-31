{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad
import DepLib
import Fixer


dummyEntry :: Entry
dummyEntry = Entry {entryRaw = d, entryLemma=d, entryPos = d, entryXPos = d, entryMisc = "", entryFeatures="", entryParent=0, entryLabel = "ROOT"}
 where d = "dummy"

main :: IO ()
main = do
  inputs <- getArgs
  ss <- parseManyFiles (parseNivreSentences False) inputs
  forM_ ss $ \s0 -> do
   case s0 of 
    Nothing -> L.putStrLn (showEntryWithEnhancement 1 dummyEntry []) 
    Just s -> do
      let enh = consolidateArcs . allEnhancer $ s
          ls = zipWith3 showEntryWithEnhancement [1..] s enh
      forM_ ls L.putStrLn
   putStrLn ""

