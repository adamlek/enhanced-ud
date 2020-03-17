{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Fixer where

import DepLib
import qualified Data.ByteString.Lazy.Char8 as L

data Arc = Arc {arcSource, arcTarget :: Int, arcLabel :: L.ByteString }

-- isSubject :: Entry -> Bool
-- isSubject Entry{..} = entryLabel == "nsubj"

kidsOf :: Int -> Sentence -> [(Int,Entry)]
kidsOf parent es = [(i,e) | (i,e@Entry{..}) <- zip numbering es, entryParent == parent]

numbering :: [Int]
numbering = [1..]

conjFixer1 :: Sentence -> [Arc]
conjFixer1 es = concat $ zipWith enhancer numbering es
  where enhancer eIndex e
          = [Arc {arcSource = kidIndex,
                  arcTarget = entryParent e,
                  arcLabel = entryLabel e}
            | entryLabel e `elem` ["nsubj","obj"],
              (kidIndex,kid) <- kidsOf eIndex es,
              entryLabel kid == "conj"]

