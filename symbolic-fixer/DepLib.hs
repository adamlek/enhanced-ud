{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}
module DepLib where
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString as L
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Char
import Data.List.Split
import Data.List (intercalate)

type Dict = M.Map T.Text Int

data Entry = Entry {entryRaw :: !T.Text,
                    entryLemma :: !T.Text,
                    entryPos :: !T.Text,
                    entryXPos :: !T.Text,
                    entryFeatures :: ![T.Text],
                    entryParent :: !Int,
                    entryLabel :: !T.Text,
                    entryMisc :: !T.Text}
             deriving Show
type Sentence = [Entry]

entryWord :: Dict -> Entry ->  Int
entryWord m Entry{..} = case (M.lookup entryRaw m,M.lookup entryPos m) of
  (Just x,_) -> x
  (_,Just x) -> x
  _ -> error ("Input word not found in vocabulary: " ++ show (entryRaw, entryPos))

showSentence' :: Dict -> Sentence -> String
showSentence' intMap = intercalate " " . map (show . entryWord intMap)

showSentence :: Dict -> Sentence -> T.Text
showSentence intMap = T.intercalate (T.pack " ") . map (T.pack . show . entryWord intMap)

showRoles :: Dict -> Sentence -> T.Text
showRoles intMap = T.intercalate (T.pack " ") . map (T.pack . show . (\k -> M.findWithDefault 0 k intMap) . entryLabel)

showSentenceText :: Sentence -> T.Text
showSentenceText = T.intercalate (T.pack " ") . map entryRaw




getRaw :: Entry -> (T.Text, Int)
getRaw Entry{..} = (entryRaw,1::Int)
getRawOrPos :: M.Map T.Text a -> Entry -> (T.Text, Int)
getRawOrPos m Entry{..} = (,1::Int) $ case M.lookup entryRaw m of
  Just _ -> entryRaw
  Nothing -> entryPos

getRawAndPos :: Entry -> [(T.Text, Int)]
getRawAndPos Entry{..} = (,1::Int) <$> [entryRaw, entryPos]


showDictEntry :: (T.Text,Int) -> T.Text
showDictEntry (wd,num) = tabSep [wd,bshow num]

readDictEntry :: T.Text -> (T.Text,Int)
readDictEntry e = (wd,read (T.unpack num))
  where (wd,num) = case T.split (== '\t') e of
          [x,y] -> (x,y)
          _ -> error $ "Could not parse dict entry: " ++ show e

-- readManyUTFFiles :: [String] -> IO T.Text
-- readManyUTFFiles fns = do
--   mconcat <$> (forM fns T.readFile)

isVerb :: Entry -> Bool
isVerb Entry{..} = entryPos `elem` ["VERB", "AUX"]

tabSep :: [T.Text] -> T.Text
tabSep = T.intercalate "\t"
lineSep :: [T.Text] -> T.Text
lineSep = T.concat . map (<> T.pack "\n")

splitLines :: T.Text -> [T.Text]
splitLines = T.split (== '\n')

bshow :: Int -> T.Text
bshow = T.pack . show

bread :: Read a => T.Text -> a
bread = read . T.unpack

ignoreLine :: T.Text -> Bool
ignoreLine l = T.null l || '#' == (T.head l)

allOk :: [CleanedEntry] -> Maybe [Entry]
allOk [] = Just []
allOk (OkEntry x:xs) = (x:) <$> allOk xs
allOk (_:_) = Nothing


data CleanedEntry = UnknownEntry | DashEntry | OkEntry Entry

dashEntry :: CleanedEntry -> Bool
dashEntry DashEntry = True
dashEntry _ = False

parseNivreSentences :: Bool -> T.Text -> [Maybe Sentence]
parseNivreSentences lcase =
  map (allOk . filter (not . dashEntry) . map (cleanEntry . T.split (== '\t'))) .
  filter (not . null) .
  map (filter (not . ignoreLine)) .
  splitWhen (T.null) .
  filter (/= T.pack "(())") .
  splitLines
  where cleanEntry :: [T.Text] -> CleanedEntry
        cleanEntry [_index,raw,lemma,pos,xpos,features,parent,label,_,misc] =
          case T.all isDigit parent of
            False -> case T.all (\c -> isDigit c || c == '-') parent of
                       True -> DashEntry
                       False -> UnknownEntry
            True -> OkEntry Entry {entryParent = bread parent
                ,entryRaw = (if lcase then T.map toLower else id) raw
                ,entryLemma = (if lcase then T.map toLower else id) lemma
                ,entryPos = pos
                ,entryXPos = xpos
                ,entryFeatures = T.split (== '|') features
                ,entryMisc=misc
                ,entryLabel = label}
        cleanEntry x = error $ "CleanEntry: " ++ show x

parseManyFiles :: (T.Text -> [a]) -> [String] -> IO [a]
parseManyFiles parser fns = mconcat <$>
  (forM fns $ \fn ->
      (do bs <- L.readFile fn
          let f = T.decodeUtf8With T.lenientDecode bs
          return (parser f)))
