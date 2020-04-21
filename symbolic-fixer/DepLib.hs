{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}
module DepLib where
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Monoid
import Data.Char
import Data.List.Split
import Data.List (intercalate)

type Dict = M.Map L.ByteString Int

data Entry = Entry {entryRaw :: !L.ByteString,
                    entryLemma :: !L.ByteString,
                    entryPos :: !L.ByteString,
                    entryXPos :: !L.ByteString,
                    entryFeatures :: ![L.ByteString],
                    entryParent :: !Int,
                    entryLabel :: !L.ByteString,
                    entryMisc :: !L.ByteString}
             deriving Show
type Sentence = [Entry]

entryWord :: Dict -> Entry ->  Int
entryWord m Entry{..} = case (M.lookup entryRaw m,M.lookup entryPos m) of
  (Just x,_) -> x
  (_,Just x) -> x
  _ -> error ("Input word not found in vocabulary: " ++ show (entryRaw, entryPos))

showSentence' :: Dict -> Sentence -> String
showSentence' intMap = intercalate " " . map (show . entryWord intMap)

showSentence :: Dict -> Sentence -> L.ByteString
showSentence intMap = L.intercalate (L.pack " ") . map (L.pack . show . entryWord intMap)

showRoles :: Dict -> Sentence -> L.ByteString
showRoles intMap = L.intercalate (L.pack " ") . map (L.pack . show . (\k -> M.findWithDefault 0 k intMap) . entryLabel)

showSentenceText :: Sentence -> L.ByteString
showSentenceText = L.intercalate (L.pack " ") . map entryRaw




getRaw Entry{..} = (entryRaw,1::Int)
getRawOrPos m Entry{..} = (,1::Int) $ case M.lookup entryRaw m of
  Just _ -> entryRaw
  Nothing -> entryPos

getRawAndPos :: Entry -> [(L.ByteString, Int)]
getRawAndPos Entry{..} = (,1::Int) <$> [entryRaw, entryPos]


showDictEntry :: (L.ByteString,Int) -> L.ByteString
showDictEntry (wd,num) = tabSep [wd,bshow num]

readDictEntry :: L.ByteString -> (L.ByteString,Int)
readDictEntry e = (wd,read (L.unpack num))
  where (wd,num) = case L.split '\t' e of
          [x,y] -> (x,y)
          _ -> error $ "Could not parse dict entry: " ++ show e

readManyUTFFiles :: [String] -> IO L.ByteString
readManyUTFFiles fns = do
  mconcat <$> (forM fns L.readFile)

isVerb Entry{..} = entryPos `elem` ["VERB", "AUX"]

tabSep :: [L.ByteString] -> L.ByteString
tabSep = L.intercalate "\t"
lineSep :: [L.ByteString] -> L.ByteString
lineSep = L.concat . map (<> L.pack "\n")

splitLines :: L.ByteString -> [L.ByteString]
splitLines = L.split '\n'

bshow :: Int -> L.ByteString
bshow = L.pack . show

bread :: Read a => L.ByteString -> a
bread = read . L.unpack

ignoreLine :: L.ByteString -> Bool
ignoreLine l = L.null l || '#' == (L.head l)

allJust :: [Maybe a] -> Maybe [a]
allJust [] = Just []
allJust (Nothing:_) = Nothing
allJust (Just x:xs) = (x:) <$> allJust xs

parseNivreSentences :: Bool -> L.ByteString -> [Maybe Sentence]
parseNivreSentences lcase =
  map (allJust . map (cleanEntry . L.split '\t')) .
  filter (not . null) .
  map (filter (not . ignoreLine)) .
  splitWhen (L.null) .
  filter (/= L.pack "(())") .
  splitLines
  where cleanEntry :: [L.ByteString] -> Maybe Entry
        cleanEntry [_index,raw,lemma,pos,xpos,features,parent,label,_,misc] =
          case L.all isDigit parent of
            False -> Nothing
            True -> Just Entry {entryParent = bread parent
                ,entryRaw = (if lcase then L.map toLower else id) raw
                ,entryLemma = (if lcase then L.map toLower else id) lemma
                ,entryPos = pos
                ,entryXPos = xpos
                ,entryFeatures = L.split '|' features
                ,entryMisc=misc
                ,entryLabel = label}
        cleanEntry x = error $ "CleanEntry: " ++ show x

parseManyFiles :: (L.ByteString -> [a]) -> [String] -> IO [a]
parseManyFiles parser fns = mconcat <$> (forM fns $ \fn -> (do f <- L.readFile fn; return (parser f)))
