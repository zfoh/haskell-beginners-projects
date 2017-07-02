{-# LANGUAGE OverloadedStrings #-}

module Memegen.Lib
    ( memegenEntry
    ) where

import           Control.Concurrent (withMVar)
import           Control.Lens ((^#))
import           Control.Monad.State (liftM, liftIO)
import           Data.Aeson (encode)
import           Data.Int (Int64(..))
import           Data.Map.Lazy ((!))
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Text.Encoding (decodeUtf8)
import           Memegen.App (AppState(..), db)
import           Memegen.Img (createMeme)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Memegen.Db as Db
import qualified Snap as S
import qualified Snap.Snaplet.SqliteSimple as S
import qualified Snap.Util.FileServe as S
import qualified Snap.Util.FileUploads as S
import qualified System.IO.Streams as IOS
import           Snap.Core (Method(..), rqPostParams, getRequest, writeBS, getParam, method)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))

uploadDir :: String
uploadDir = "upload"

maxFileSize :: Int64
maxFileSize = 2^(24::Int)  -- 16MB

memegenEntry :: IO ()
memegenEntry = S.serveSnaplet S.defaultConfig appInit

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

-- | The application's routes.
routes :: [(B.ByteString, S.Handler AppState AppState ())]
routes = [ ("/", S.ifTop $ S.writeBS "hello there")
         , ("hello/:echoparam", method GET $ echoHandler)
         , ("upload", method POST $ uploadHandler)
         , ("list", method GET $ listHandler)
         , ("image", S.serveDirectory uploadDir)
         ]

echoHandler :: S.Handler AppState AppState ()
echoHandler = do
  Just param <- getParam "echoparam"
  writeBS $ B.append "Hello " param

listHandler :: S.Handler AppState AppState ()
listHandler = do
  memes <- S.withTop db $ Db.listMemes
  writeBS $ BL.toStrict $ encode memes

uploadHandler :: S.Handler AppState AppState ()
uploadHandler = do
  -- Files are sent as HTTP multipart form entries.
  files <- S.handleMultipart uploadPolicy $ \part istream -> do
    content <- IOS.fold B.append B.empty istream
    return (part, content)
  let (imgPart, imgContent) = head files
  let fileName = fromJust (S.partFileName imgPart)

  req <- getRequest
  let params = rqPostParams req
  let topText = decodeUtf8 $ head (params ! "top")
  let bottomText = decodeUtf8 $ head (params ! "bottom")

  -- Create a meme
  memeContent <- liftIO $
    createMeme imgContent (T.unpack topText) (T.unpack bottomText)
  -- Store the meme metadata into DB
  S.withTop db $ Db.saveMeme topText bottomText (decodeUtf8 fileName)
  -- Store the image in upload directory.
  -- writeFile operates inside IO monad. Snap handlers run inside Snap
  -- monad, which provides an access to IO monad. We use liftIO to
  -- execute a function inside IO monad and return to Snap monad.
  liftIO $ B.writeFile
    (uploadDir </> (T.unpack $ decodeUtf8 fileName)) memeContent

  writeBS memeContent

  where
    uploadPolicy :: S.UploadPolicy
    uploadPolicy =
      S.setMaximumFormInputSize (maxFileSize) S.defaultUploadPolicy
