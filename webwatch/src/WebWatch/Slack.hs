-- | This module contains code for sending notification mails.
{-# LANGUAGE OverloadedStrings #-}
module WebWatch.Slack
    ( sendLinks
    ) where

import           Data.Aeson              ((.=))
import qualified Data.Aeson              as Aeson
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http
import           WebWatch.GetLinks

data Payload = Payload
    { payloadText :: !T.Text
    } deriving (Show)

instance Aeson.ToJSON Payload where
    toJSON p = Aeson.object
        [ "text" .= payloadText p
        ]

sendLinks
    :: T.Text -- ^ Special access URL
    -> [Link] -- ^ Links
    -> IO ()
sendLinks webhookUrl links = do
    -- Create payload
    let payload = mkPayload links

    -- Initialize an HTTP manager
    manager <- Http.newManager Http.tlsManagerSettings

    -- We create an initial request by parsing the URL
    request0 <- Http.parseRequest (T.unpack webhookUrl)

    -- But we need to set a few more details:
    let request = request0
            { Http.method      = "POST"
            , Http.requestBody = Http.RequestBodyLBS (Aeson.encode payload)
            }

    -- We perform the request (but we can ignore the result).
    _ <- Http.httpLbs request manager

    return ()

mkPayload
    :: [Link]
    -> Payload
mkPayload links = Payload
    { payloadText = T.unlines $
        [ "We found a few links matching your query."
        , ""
        ] ++
        [ "- " <> lTitle link <> "\n  " <> lHref link
        | link <- links
        ]
    }
