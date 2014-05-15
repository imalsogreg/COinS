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
        , rAu          :: Maybe PairValue
        , rAuCorp      :: Maybe PairValue
        , rISBN        :: Maybe PairValue
        , rCoden       :: Maybe PairValue
        , rSICI        :: Maybe PairValue
        , rGenre       :: Maybe PairValue
        , rChron       :: Maybe PairValue
        , rSsn         :: Maybe PairValue
        , rQuarter     :: Maybe PairValue
        , rPart        :: Maybe PairValue
        }

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

unescapeCOinS :: BS.ByteString -> BS.ByteString
unescapeCOinS = Text.encodeUtf8 .
                foldl f id [("%20"," "),("%23","#"),("%25","%"),("%26","&")
                           ,("%2B","+"),("%2F","/"),("%3C","<"),("%3D","=")
                           ,("%3E",">"),("%3F","?"),("%3A",":")
                           ,("+"," "),("&amp;","&"),("%E2%80%94","―")] .
                Text.decodeUtf8
  where f :: (Text.Text -> Text.Text) -> (Text.Text, Text.Text) -> (Text.Text -> Text.Text)
        f fAcc (tFrom,tTo) = fAcc . Text.replace tFrom tTo


parseEscapedCOinS :: String -> Either ParseError COinS
parseEscapedCOinS s = do
  p <- parse coinsParser "COinS string" s
  return $ pairsToCOinS p

pairsToCOinS :: [(BS.ByteString, BS.ByteString)] -> COinS
pairsToCOinS = undefined

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
