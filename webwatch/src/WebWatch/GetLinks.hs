-- | This module is responsible for scraping for links on a page containing
-- certain keywords.
{-# LANGUAGE OverloadedStrings #-}
module WebWatch.GetLinks
    ( Link (..)
    , getMatchingLinks
    ) where

import           Data.Maybe              (mapMaybe)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.HTTP.Conduit    as Http (parseUrl)
import qualified Network.HTTP.Simple     as Http
import qualified Network.URI             as Uri
import qualified Text.HTML.TagSoup       as TagSoup

data Link = Link
    { lTitle :: !T.Text
    , lHref  :: !T.Text
    } deriving (Show)

matchingLinks :: [T.Text] -> [Link] -> [Link]
matchingLinks patterns = filter $
    \l -> any (`T.isInfixOf` T.toLower (lTitle l)) lpatterns
  where
    lpatterns = map T.toLower patterns

extractLinks :: T.Text -> [Link]
extractLinks = findLinks . TagSoup.parseTags
  where
    findLinks (TagSoup.TagOpen "a" args : tags0) =
        let closeLink = (== TagSoup.TagClose "a") in
        case (lookup "href" args, break closeLink tags0) of
            (Nothing,   (_,     tags1))       -> findLinks tags1
            (_,         (_,     []))          -> []
            (Just href, (title, (_ : tags1))) ->
                Link (TagSoup.innerText title) href : findLinks tags1

    findLinks (_ : tags0) = findLinks tags0
    findLinks [] = []

makeAbsolute :: T.Text -> [Link] -> [Link]
makeAbsolute url = mapMaybe $ \l -> do
    base <- baseUri
    href <- Uri.parseURI $ T.unpack (lHref l)
    return l {lHref = T.pack $ show $ href `Uri.relativeTo` base}
  where
    baseUri = Uri.parseURI $ T.unpack url

getMatchingLinks
    :: [T.Text]
    -> T.Text
    -> IO [Link]
getMatchingLinks patterns uri = do
    req <- Http.parseUrl (T.unpack uri)
    lbs <- Http.getResponseBody <$> Http.httpLbs req
    return $
        makeAbsolute uri $
        matchingLinks patterns $
        extractLinks $
        TL.toStrict $ TL.decodeUtf8 lbs
