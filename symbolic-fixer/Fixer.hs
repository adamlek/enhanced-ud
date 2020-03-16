{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Fixer where

import DepLib
import qualified Data.ByteString.Lazy.Char8 as L

data Arc = Arc {arcSource, arcTarget :: Int, arcLabel :: L.ByteString }
isSubject :: Entry -> Bool
isSubject Entry{..} = entryLabel == "nsubj"

kidsOf :: Int -> Sentence -> [(Int,Entry)]
kidsOf parent es = [(i,e) | (i,e@Entry{..}) <- zip [0..] es, entryParent == parent]

conjFixer1 :: Sentence -> [Arc]
conjFixer1 es = concat $ zipWith enhancer [0..] es
  where enhancer eIndex e
          = [Arc {arcSource = kidIndex,
                  arcTarget = entryParent e,
                  arcLabel = "nsubj"}
            | isSubject e,
              (kidIndex,kid) <- kidsOf eIndex es,
              entryLabel kid == "conj"]
