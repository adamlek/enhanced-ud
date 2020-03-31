{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Fixer where
import Data.List
import Data.Function
import DepLib
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

data Arc = Arc {arcSource, arcTarget :: Int, arcLabel :: L.ByteString }
         | DeleteOld {arcTarget :: Int, arcLabel :: L.ByteString} deriving Show

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


-- | Examples 5->6 and and 7->8.
-- Have:
-- was <----- reading -----> watching
--      aux           conj
-- She <----- Reading -----> Watching
--      nsubj          conj
-- To add:
--     <-------------------
--             nsubj
--              aux
-- ATTENTION: if the conjoined verb already has a subject, then that one is the subject, and we should not make a modification. Example:
-- She <----- Reading -----> Watching ------> He
--      nsubj          conj            nsubj
-- ALSO:
-- She was reading and he watched the television. (even though there is no auxilary in one sentence nothing must be propagated.)

               
conjEnhancer2 :: Sentence -> [Arc]
conjEnhancer2 es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e
          = [Arc {arcSource = kidIndex,
                  arcTarget = eIndex,
                  arcLabel = entryLabel e}
            | entryLabel e `elem` ["nsubj", "aux"],
              -- eIndex e ~ she
              -- entryParent e ~ reading
              (kidIndex,kid) <- kidsOf (entryParent e) es,
              -- kidIndex ~ watching
              all ((/= "nsubj") . entryLabel . snd) (kidsOf kidIndex es),
              -- if the conjoined verb has a subject aleardy, then we
              -- are conjoining full sentences. There is no need to propagate *anything*
              entryLabel kid == "conj"]

-- | Examples 11->12; 13->14; 15->16; 17->18; 21->22.
-- Have:
-- tags: nsubj, obj, amod
-- Meet -----> Paul -----> Mary
--      nsubj        conj
-- have -----> founded -----> leader 
--      advcl           conj
--   mounted -----> Tikrit  -----> Huwaijah
--            obl             conj

conjEnhancer :: Sentence -> [Arc]
conjEnhancer es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e
          = [Arc {arcTarget = kidIndex,
                  arcSource = entryParent e,
                  arcLabel = entryLabel e}
            | entryLabel e `elem` ["nsubj","obj", "amod", "advcl","obl"],
              (kidIndex,kid) <- kidsOf eIndex es,
              entryLabel kid == "conj"]

-- | Example 23->24; 25->26
-- interior <------ look ------> new
--           nsubj        xcomp
xcompEnhancer :: Sentence -> [Arc]
xcompEnhancer es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e
          = [Arc {arcTarget = e2Index,
                  arcSource = eIndex,
                  arcLabel = "nsubj:xsubj"}
            | entryLabel e `elem` ["xcomp"],
              (e2Index,e2) <- zip numbering es,
              entryLabel e2 == "nsubj",
              entryParent e2 == entryParent e]

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
              DeleteOld kidIndex "nsubj"]
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


-- From <------ AP <------- come
--        case        obl
--                    obl:from

-- and <------ patience <------- hope
--        cc              conj
--                        conj:and

-- since <----- leader <--------- have
--        mark            advcl
--        mark            advcl:since

labelingMap :: M.Map L.ByteString [L.ByteString]
labelingMap = M.fromList
   [("case", ["obl","nmod"]),
    ("cc",["conj"]),
    ("mark",["advcl", "acl"])
    ]


fixCase :: [Entry] -> [Arc]
fixCase es = concat $ zipWith enhancer numbering es
  where enhancer _eIndex e = concat
         [[Arc {arcLabel = entryLabel parentEntry <> ":" <> entryLemma e
               ,arcSource = entryParent parentEntry
               ,arcTarget = entryParent e},
              DeleteOld (entryParent e) (entryLabel parentEntry)]
         | Just enhancedLabels <- [M.lookup (entryLabel e) labelingMap],
           -- eIndex ~ From
           -- entryParent e ~ AP
           let parentEntry = parentOf (entryParent e) es, -- all info about "AP"
           entryLabel parentEntry `elem` enhancedLabels
           -- entryParent parentEntry ~ come
         ]

parentOf :: Int -> [a] -> a
parentOf idx es = es !! (idx-1)

matchingArc :: L.ByteString -> Int -> Arc -> Bool
matchingArc lab x Arc{..}  = arcTarget == x && arcLabel == lab
matchingArc _ _ _ = False

applyDelete :: [Arc] -> [Arc]
applyDelete (DeleteOld x lab:xs) = filter (not . matchingArc lab x) (applyDelete xs)
applyDelete (x:xs) = x:applyDelete xs
applyDelete [] = []

allEnhancer :: Sentence -> [Arc]
allEnhancer s =
  applyDelete (fixCase s ++ relEnhancer s ++ xcompEnhancer s ++ conjEnhancer s ++ conjEnhancer2 s ++ sentenceToArcs s)

noEnhancer :: Sentence -> [Arc]
noEnhancer = sentenceToArcs

type Enhancement = [(Int,L.ByteString)] -- list of parent/head and label.

showEnhancement :: Enhancement -> L.ByteString
showEnhancement [] = "(incorrect input)"
showEnhancement xs = foldr1 (\x y -> x <> "|" <> y) . map showSemiArc $ xs
  where showSemiArc :: (Int,L.ByteString) -> L.ByteString
        showSemiArc (i,label) = bshow i <> ":" <> label

consolidateArcs :: [Arc] -> [Enhancement]
consolidateArcs = map (sort . map arcToEnhancement) . groupBy ((==) `on` arcTarget) . sortBy (compare `on` arcTarget)

showEntryWithEnhancement :: Int -> Entry -> Enhancement -> L.ByteString
showEntryWithEnhancement index Entry{..} enh =
  L.intercalate "\t" [bshow index,entryRaw, entryLemma,entryPos,
                      entryXPos,entryFeatures,bshow entryParent,entryLabel,
                      showEnhancement enh,entryMisc]

arcToEnhancement :: Arc -> (Int, L.ByteString)
arcToEnhancement a = (arcSource a,arcLabel a)

-- 12	staff	staff	NOUN	NN	Number=Sing	7	conj	4:obj|7:conj:and	_
