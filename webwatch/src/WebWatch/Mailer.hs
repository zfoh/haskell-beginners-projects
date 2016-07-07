-- | This module contains code for sending notification mails.
{-# LANGUAGE OverloadedStrings #-}
module WebWatch.Mailer
    ( sendLinks
    ) where

import           Control.Lens      ((&), (.~))
import           Data.Monoid       ((<>))
import qualified Data.Text         as T
import qualified Network.AWS       as Aws
import qualified Network.AWS.SES   as Aws.Ses
import           System.IO         (stderr)
import           WebWatch.GetLinks

sendLinks
    :: T.Text -- ^ From
    -> T.Text -- ^ To
    -> [Link] -- ^ Links
    -> IO ()
sendLinks from to links = do
    lgr <- Aws.newLogger Aws.Error stderr
    env <- Aws.newEnv Aws.NorthVirginia Aws.Discover
    Aws.runResourceT $ Aws.runAWS (env & Aws.envLogger .~ lgr) $ do
        _ <- Aws.send $ Aws.Ses.sendEmail
            from
            (Aws.Ses.destination & Aws.Ses.dToAddresses .~ [to])
            (mkMessage links)
        return ()

mkMessage
    :: [Link]
    -> Aws.Ses.Message
mkMessage links = Aws.Ses.message
    (Aws.Ses.content $ mkSubject links)
    (Aws.Ses.body & Aws.Ses.bText .~ Just (Aws.Ses.content $ mkBody links))

mkSubject
    :: [Link]
    -> T.Text
mkSubject links = "WebWatch: got " <> T.pack (show $ length links) <> " links"

mkBody
    :: [Link]
    -> T.Text
mkBody links = T.unlines $
    [ "Hey brozinsky!"
    , ""
    , "We found a few links matching your query."
    , ""
    ] ++
    [ "- " <> lTitle link <> "\n  " <> lHref link
    | link <- links
    ] ++
    [ ""
    , "We hope you have a good day!"
    , ""
    , "Mr. WebWatch"
    ]

