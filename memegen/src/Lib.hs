{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( memegenEntry
    ) where

import           App (AppState(..), db)
import           Control.Concurrent (withMVar)
import           Control.Lens ((^#))
import           Control.Monad.State (liftM, liftIO)
import           Data.Aeson (encode)
import           Data.Int (Int64(..))
import           Data.Map.Lazy ((!))
import           Data.Maybe (fromJust)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Img (createMeme)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import qualified Db
import qualified Snap as S
import qualified Snap.Snaplet.SqliteSimple as S
import qualified Snap.Util.FileServe as S
import qualified Snap.Util.FileUploads as S
import           Snap.Core (Method(..), rqPostParams, getRequest, writeBS, getParam, writeText, method)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))


uploadDir :: String
uploadDir = "upload"

maxFileSize :: Int64
maxFileSize = 2^(24::Int)  -- 16MB

memegenEntry :: IO ()
memegenEntry = S.serveSnaplet S.defaultConfig appInit

-- | The application's routes.
routes :: [(B.ByteString, S.Handler AppState AppState ())]
routes = [ ("/", writeText "hello there")
         , ("hello/:echoparam", method GET $ echoHandler)
         , ("upload", method POST $ uploadHandler)
         , ("image", S.serveDirectory uploadDir)
         ]

echoHandler :: S.Handler AppState AppState ()
echoHandler = method GET doHandle
  where
    doHandle = do
      param <- getParam "echoparam"
      maybe (writeBS "must specify echo/param in URL")
            (writeBS . (B.append "Hello ")) param

uploadHandler :: S.Handler AppState AppState ()
uploadHandler = method POST doUpload
  where
    doUpload = do
        files <- S.handleMultipart uploadPolicy $ \part -> do
          content <- liftM B.concat EL.consume
          return (part, content)
        let (imgPart, imgContent) = head files
        let fileName = fromJust (S.partFileName imgPart)

        req <- getRequest
        let params = rqPostParams req
        let upperText = decodeUtf8 $ head (params ! "upper")
        let lowerText = decodeUtf8 $ head (params ! "lower")

        -- Create a meme
        memeContent <- liftIO $
          createMeme imgContent (T.unpack upperText) (T.unpack lowerText)
        -- Store the meme metadata into DB
        S.withTop db $ Db.saveMeme upperText lowerText (decodeUtf8 fileName)
        -- Store the image
        liftIO $ B.writeFile
          (uploadDir </> (T.unpack $ decodeUtf8 fileName)) memeContent

        memes <- S.withTop db $ Db.listMemes
        writeBS $ BL.toStrict $ encode memes

        where
          uploadPolicy :: S.UploadPolicy
          uploadPolicy =
            S.setMaximumFormInputSize (maxFileSize) S.defaultUploadPolicy

-- | The application initializer.
appInit :: S.SnapletInit AppState AppState
appInit = S.makeSnaplet "memegen" "Meme generator." Nothing $ do
    S.addRoutes routes
    d <- S.nestSnaplet "db" db S.sqliteInit

    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = S.sqliteConn $ d ^# S.snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn

    -- Create upload directory
    liftIO $ createDirectoryIfMissing True uploadDir

    return $ AppState d
