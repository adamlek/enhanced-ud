{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Fixer where
import Data.List
import Data.Function
import DepLib
import qualified Data.ByteString.Lazy.Char8 as L

data Arc = Arc {arcSource, arcTarget :: Int, arcLabel :: L.ByteString }
         | DeleteOld {arcTarget :: Int} deriving Show

-- isSubject :: Entry -> Bool
-- isSubject Entry{..} = entryLabel == "nsubj"

-- | kidsOf b = [a,c]
-- a <----- b -----> c
kidsOf :: Int -> Sentence -> [(Int,Entry)]
kidsOf parent es = [(i,e) | (i,e) <- zip numbering es, entryParent e == parent]

numbering :: [Int]
numbering = [1..]

sentenceToArcs :: [Entry] -> [Arc]
sentenceToArcs es = zipWith entryToArc numbering es

entryToArc :: Int -> Entry -> Arc
entryToArc i e = Arc {arcSource = entryParent e, arcTarget = i, arcLabel = entryLabel e}

-- See https://universaldependencies.org/u/overview/enhanced-syntax.html for documentation

-- type 1
-- tags: nsubj, obj, amod
-- Meet -----> Paul -----> Mary
--      nsubj        conj
conjEnhancer :: Sentence -> [Arc]
conjEnhancer es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e
          = [Arc {arcTarget = kidIndex,
                  arcSource = entryParent e,
                  arcLabel = entryLabel e}
            | entryLabel e `elem` ["nsubj","obj", "amod"],
              (kidIndex,kid) <- kidsOf eIndex es,
              entryLabel kid == "conj"]


-- | Examples 5->6 and and 7->8.
-- Have:
-- She <----- Reading -----> Watching
--      nsubj          conj
-- To add:
--     <-------------------
--             nsubj
conjEnhancer2 :: Sentence -> [Arc]
conjEnhancer2 es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e
          = [Arc {arcSource = kidIndex,
                  arcTarget = eIndex,
                  arcLabel = entryLabel e}
            | entryLabel e `elem` ["nsubj","aux"],
              -- eIndex e ~ she
              -- entryParent e ~ reading
              (kidIndex,kid) <- kidsOf (entryParent e) es,
              -- kidIndex ~ watching
              entryLabel kid == entryLabel e]

-- | Examples 27 -> 30 and 33 -> 34. Not that this rule conflicts with
-- 31->32. Supposedly the choice should be determined by lexical
-- semantics.

-- boy ----------> lived -------> who
--      acl:recl          nsubj

-- TO:

-- boy ----------> lived
--      acl:recl
--     <----------
--        nsubj
--     --------------------------> who
--                 ref

relEnhancer :: Sentence -> [Arc]
relEnhancer es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e = concat
            [[Arc {arcTarget = kidIndex,
                   arcSource = entryParent e,
                   arcLabel = "ref"},
              Arc {arcTarget = entryParent e,
                   arcSource = eIndex,
                   arcLabel = relativizeLabel (entryLabel kid)},
              DeleteOld kidIndex]
            | entryLabel e `elem` ["acl:relcl"],
              -- eIndex ~ lived
              -- entryParent e ~ boy
              (kidIndex,kid) <- kidsOf eIndex es,
              entryLabel kid `elem` ["nsubj"] -- ,"obj","advmod"] -- ???
              -- kid ~ who
            ]

relativizeLabel :: L.ByteString -> L.ByteString
relativizeLabel l = if l == "advmod"
                    then "obl"
                    else l

-- type 2
-- interior <------ look ------> new
--           nsubj        xcomp
xcompEnhancer :: Sentence -> [Arc]
xcompEnhancer es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e
          = [Arc {arcTarget = e2Index,
                  arcSource = eIndex,
                  arcLabel = "xsubj"}
            | entryLabel e `elem` ["xcomp"],
              (e2Index,e2) <- zip numbering es,
              entryLabel e2 == "nsubj",
              entryParent e2 == entryParent e]

-- type 3
-- she <----- reading -----> watching
--      subj          conj
-- was <----- reading -----> watching
--      aux           conj
conj3Enhancer :: Sentence -> [Arc]
conj3Enhancer es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e
          = [Arc {arcTarget = e2Index,
                  arcSource = eIndex,
                  arcLabel = entryLabel e}
            | entryLabel e `elem` ["nsubj", "aux"],
              (e2Index,e2) <- zip numbering es,
              entryLabel e2 == "conj",
              entryParent e2 == entryParent e]

applyDelete :: [Arc] -> [Arc]
applyDelete (DeleteOld x:xs) = filter ((/= x) . arcTarget) (applyDelete xs)
applyDelete (x:xs) = x:applyDelete xs
applyDelete [] = []

allEnhancer :: Sentence -> [Arc]
allEnhancer s = applyDelete (relEnhancer s ++ xcompEnhancer s ++ conjEnhancer s ++ conjEnhancer2 s ++ sentenceToArcs s)

type Enhancement = [(Int,L.ByteString)] -- list of parent/head and label.

showEnhancement :: Enhancement -> L.ByteString
showEnhancement [] = "(incorrect input)"
showEnhancement xs = foldr1 (\x y -> x <> "|" <> y) . map showSemiArc $ xs
  where showSemiArc :: (Int,L.ByteString) -> L.ByteString
        showSemiArc (i,label) = bshow i <> ":" <> label

consolidateArcs :: [Arc] -> [Enhancement]
consolidateArcs = map (map arcToEnhancement) . groupBy ((==) `on` arcTarget) . sortBy (compare `on` arcTarget)

arcToEnhancement :: Arc -> (Int, L.ByteString)
arcToEnhancement a = (arcSource a,arcLabel a)

-- 12	staff	staff	NOUN	NN	Number=Sing	7	conj	4:obj|7:conj:and	_
