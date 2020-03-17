{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Fixer where
import Data.List
import Data.Function
import DepLib
import qualified Data.ByteString.Lazy.Char8 as L

data Arc = Arc {arcSource, arcTarget :: Int, arcLabel :: L.ByteString } deriving Show

-- isSubject :: Entry -> Bool
-- isSubject Entry{..} = entryLabel == "nsubj"

kidsOf :: Int -> Sentence -> [(Int,Entry)]
kidsOf parent es = [(i,e) | (i,e) <- zip numbering es, entryParent e == parent]

numbering :: [Int]
numbering = [1..]

sentenceToArcs :: [Entry] -> [Arc]
sentenceToArcs es = zipWith entryToArc numbering es

entryToArc :: Int -> Entry -> Arc
entryToArc i e = Arc {arcSource = entryParent e, arcTarget = i, arcLabel = entryLabel e}

conjEnhancer :: Sentence -> [Arc]
conjEnhancer es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e
          = [Arc {arcTarget = kidIndex,
                  arcSource = entryParent e,
                  arcLabel = entryLabel e}
            | entryLabel e `elem` ["nsubj","obj"],
              (kidIndex,kid) <- kidsOf eIndex es,
              entryLabel kid == "conj"]

allEnhancer :: Sentence -> [Arc]
allEnhancer s = conjEnhancer s ++ sentenceToArcs s

type Enhancement = [(Int,L.ByteString)] -- list of parent/head and label.

consolidateArcs :: [Arc] -> [Enhancement]
consolidateArcs = map (map arcToEnhancement) . groupBy ((==) `on` arcTarget) . sortBy (compare `on` arcTarget)

arcToEnhancement :: Arc -> (Int, L.ByteString)
arcToEnhancement a = (arcSource a,arcLabel a)

-- 12	staff	staff	NOUN	NN	Number=Sing	7	conj	4:obj|7:conj:and	_
