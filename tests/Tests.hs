{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.ByteString as BS

import Data.COinS

main :: IO ()
main = defaultMain [tests]

tests = testGroup "Empty COinS" [testCase "Parse empty string" case_emptyToEmpty]

sampleEscaped :: BS.ByteString
sampleEscaped = "ctx_ver=Z39.88-2004&amp;rft_id=info%3Adoi%2Fhttp%3A%2F%2Fdx.doi.org%2F10.1080%2F758527545&amp;rfr_id=info%3Asid%2Fcrossref.org%3Asearch&amp;rft.atitle=The+international+transmission+of+stock+market+fluctuation+between+the+developed+markets+and+the+Asian%E2%80%94Pacific+markets&amp;rft.jtitle=Applied+Financial+Economics&amp;rft.date=1992&amp;rft.volume=2&amp;rft.issue=1&amp;rft.spage=43&amp;rft.epage=47&amp;rft.aufirst=Yan-Leung&amp;rft.aulast=Cheung&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Ajournal&amp;rft.genre=article&amp;rft.au=Yan-Leung+Cheung&amp;rft.au=+Sui-choi+Mak"

emptyCOinS :: COinS
emptyCOinS = COinS Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

case_emptyToEmpty = let res = case parseEscapedCOinS "" of
                          Left _ -> False
                          Right c -> c == emptyCOinS
                    in assertEqual "empty is empty" res True
