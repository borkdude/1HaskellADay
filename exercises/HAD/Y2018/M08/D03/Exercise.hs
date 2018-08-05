-- cd exercises/HAD
-- stack ghci

{-# LANGUAGE OverloadedStrings #-}

module Y2018.M08.D03.Exercise where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe
import           Data.Time

{--
Another 'discovering the structure of JSON' Haskell exercise today.

You have the follow JSON file:
--}

exDir, newsJSON :: FilePath
exDir = "Y2018/M08/D03/"
newsJSON = "news.json"

type Date = String

-- 1. read in the JSON (unprettified) and write it out as pretty JSON

prettify :: FilePath -> FilePath -> IO ()
prettify unprettyIn prettyOut = do
  sIn <- B.readFile unprettyIn
  let vals = listVals sIn
      out = encodePretty vals
  B.writeFile prettyOut out

-- this function may be helpful for solving prettify ...

listVals :: B.ByteString -> [Value]
listVals = fromJust . decode

-- prettify (exDir ++ newsJSON) (exDir ++ "news.pretty.json")

-- 2. what structures can you see from this enprettified set?

-- ... or ... that's a tough question, let's take this approach, instead:

data Article = Art { author :: String, image :: FilePath, url :: FilePath,
                     published :: Date,  updated :: Date, article :: String, 
                     idx :: Integer, summary, title :: String }
   deriving (Eq, Show)

instance FromJSON Article where
  parseJSON = withObject "Article" $ \v -> Art
    <$> ((v .: "author_meta") >>= (.: "display_name"))
    <*> ((v .: "image") >>= (.: "url"))
    <*> v .: "link"
    <*> ((v .: "content") >>= (.: "rendered"))
    <*> v .: "date"
    <*> v .: "modified"
    <*> v .: "id"
    <*> v .: "lede"
    <*> v .: "title"

{--
The mapping from the Haskell values to the JSON is as follows:

author       ==> author_meta.display_name
image        ==> image.url
url          ==> link
article      ==> content.rendered
published    ==> date
updated      ==> modified
idx          ==> id
summary      ==> lede
title        ==> title

map the JSON to the above structure.
--}

readArticles :: FilePath -> IO [Article]
readArticles json = do
  sIn <- B.readFile json
  let vals = listVals sIn
      articles :: [Article]
      fromJSONStrict v =
        let res = fromJSON v
        in case res of
          Success a -> a
          Error s -> error s
      articles = map fromJSONStrict vals
  return $ articles

-- will listVals help here?

-- articles <- readArticles (exDir ++ newsJSON)

-- a. how many articles are there? 2

-- b. what was the max id of the article set?
-- maximum $ map idx articles == 134471
