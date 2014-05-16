{-# LANGUAGE OverloadedStrings #-}

module Data.COinS where

------------------------------------------------------------------------------
import Control.Applicative ((*>),(<*),(<$>))
import qualified Data.ByteString.Char8 as BS
import Data.Aeson
import Network.URI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.Parsec
import Text.Parsec.ByteString (Parser)

type PairValue = BS.ByteString

------------------------------------------------------------------------------
-- |Low level parse of the COinS key:value pairs
data COinS =
  COinS { referrerId   :: Maybe PairValue
        , infoDOI      :: Maybe PairValue
        , infoURL      :: Maybe PairValue
        , rArtTitle    :: Maybe PairValue
        , rTitle       :: Maybe PairValue
        , rJTitle      :: Maybe PairValue
        , rShortJTitle :: Maybe PairValue
        , rDate        :: Maybe PairValue
        , rVolume      :: Maybe PairValue
        , rIssue       :: Maybe PairValue
        , rSPage       :: Maybe PairValue
        , rEPage       :: Maybe PairValue
        , rPages       :: Maybe PairValue
        , rArtNum      :: Maybe PairValue
        , rISSN        :: Maybe PairValue
        , rEISSN       :: Maybe PairValue
        , rAuLast      :: Maybe PairValue
        , rAuFirst     :: Maybe PairValue
        , rAuInit      :: Maybe PairValue
        , rAuInit1     :: Maybe PairValue
        , rAuInitM     :: Maybe PairValue
        , rAuSuffix    :: Maybe PairValue
        , rAu          :: [PairValue]
        , rAuCorp      :: Maybe PairValue
        , rISBN        :: Maybe PairValue
        , rCoden       :: Maybe PairValue
        , rSICI        :: Maybe PairValue
        , rGenre       :: Maybe PairValue
        , rChron       :: Maybe PairValue
        , rSsn         :: Maybe PairValue
        , rQuarter     :: Maybe PairValue
        , rPart        :: Maybe PairValue
        } deriving (Eq, Show)

------------------------------------------------------------------------------
-- |Interpretation of COinS value as usable article metadata
data ArticleData =
  ArticleData { title   :: BS.ByteString
              , authors :: [BS.ByteString]
              , links   :: [ArticleLink]
              , source  :: BS.ByteString
              } deriving (Show)

------------------------------------------------------------------------------
-- |Differentiate between regural links and DOI-based links
data ArticleLink = LinkURI URI
                 | LinkDOI URI
                 deriving (Eq, Show)


------------------------------------------------------------------------------
parseKeyVal :: Parser [(BS.ByteString,BS.ByteString)]
parseKeyVal = parseKeyValPair `sepBy` char '&'

parseKeyValPair :: Parser (BS.ByteString, BS.ByteString)
parseKeyValPair = do
  k <- many $ noneOf ":"
  v <- (char ':' *> many (noneOf "&"))
  return (BS.pack k, BS.pack v)

------------------------------------------------------------------------------
unescapeCOinS :: BS.ByteString -> BS.ByteString
unescapeCOinS = Text.encodeUtf8 .
                foldl f id [("%20"," "),("%23","#"),("%25","%"),("%26","&")
                           ,("%2B","+"),("%2F","/"),("%3C","<"),("%3D","=")
                           ,("%3E",">"),("%3F","?"),("%3A",":")
                           ,("+"," "),("&amp;","&"),("%E2%80%94","―")] .
                Text.decodeUtf8
  where f fAcc (tFrom,tTo) = fAcc . Text.replace tFrom tTo


parseEscapedCOinS :: String -> Either ParseError COinS
parseEscapedCOinS s = do
  p <- parse coinsParser "COinS string" s
  return $ pairsToCOinS p

------------------------------------------------------------------------------
pairsToCOinS :: [(BS.ByteString, BS.ByteString)] -> COinS
pairsToCOinS p = COinS
                 ((lookup "rfr_id=info" p) :: Maybe BS.ByteString)
                 (lookup "rft_id=info" p)
                 (lookup "rft_id=http" p)
                 (lookup "rft.article" p)
                 (lookup "rft.title" p)
                 (lookup "rft.jtitle" p)
                 (lookup "rft.stitle" p)
                 (lookup "rft.date" p)
                 (lookup "rft.volume" p)
                 (lookup "rft.issue" p)
                 (lookup "rft.spage" p)
                 (lookup "rft.epage" p)
                 (lookup "rft.pages" p)
                 (lookup "rft.artnum" p)
                 (lookup "rft.issn" p)
                 (lookup "rft.eissn" p)
                 (lookup "rft.aulast" p)
                 (lookup "rft.aufirst" p)
                 (lookup "rft.auinit" p)
                 (lookup "rft.auinit1" p)
                 (lookup "rft.auinitm" p)
                 (lookup "rft.ausuffix" p)
                 (map snd . filter ((=="rft.au") . fst) $ p) 
                 (lookup "rft.aucorp" p)
                 (lookup "rft.isbn" p)
                 (lookup "rft.coden" p)
                 (lookup "rft.sici" p)
                 (lookup "rft.genre" p)
                 (lookup "rft.chron" p)
                 (lookup "rft.ssn" p)
                 (lookup "rft.quarter" p)
                 (lookup "rft.part" p)

coinsParser :: Parsec String () [(BS.ByteString,BS.ByteString)]
coinsParser = parsePair `sepBy` char '&'

parsePair :: Parsec String () (BS.ByteString,BS.ByteString)
parsePair = do
  k <- many (noneOf "=")
  _ <- char '='
  v <- many (noneOf "&")
  return (BS.pack k, BS.pack v)
  
{-
bsReplaceAll :: BS.ByteString -> BS.ByteString -> BS.ByteString
                -> BS.ByteString
bsReplaceAll targetS newS sourceS = case BS.breakSubstring targetS sourceS of
  (f,"") -> f
  (f,s ) -> BS.append f $
            bsReplaceAll targetS newS
            (BS.append newS . BS.drop (BS.length targetS) $ s)
-}
