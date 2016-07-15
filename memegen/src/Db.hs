{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Db where

import           App (AppState(..))
import           Control.Monad.State (unless)
import qualified Database.SQLite.Simple as S
import qualified Data.Text as T
import qualified Snap.Snaplet.SqliteSimple as L
import           Snap.Snaplet (Handler(..))
import Data.Aeson
import Data.Aeson.TH

data Meme = Meme
  {
    memeId :: Int
  , upperText :: T.Text
  , lowerText :: T.Text
  , imageFilepath :: T.Text
  } deriving (Show)

$(deriveJSON defaultOptions ''Meme)

instance L.FromRow Meme where
  fromRow = Meme <$> L.field <*> L.field <*> L.field <*> L.field

tableExists :: S.Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- S.query conn "SELECT name FROM sqlite_master \
                    \WHERE type='table' AND name=?" (L.Only tblName)
  case r of
    [L.Only (_ :: String)] -> return True
    _ -> return False

-- | Create the necessary database tables, if not already initialized.
createTables :: S.Connection -> IO ()
createTables conn = do
  schemaCreated <- tableExists conn "memes"
  unless schemaCreated $
    S.execute_ conn
      (S.Query $ "CREATE TABLE memes (\
                 \id INTEGER PRIMARY KEY, \
                 \upper_text TEXT, \
                 \lower_text TEXT, \
                 \image_filepath TEXT)")

-- | Retrieve all memes.
listMemes :: Handler AppState L.Sqlite [Meme]
listMemes = L.query "SELECT id, upper_text, lower_text, image_filepath \
                    \FROM memes ORDER BY id DESC" ()

-- | Save a new meme
saveMeme :: T.Text -> T.Text -> T.Text -> Handler AppState L.Sqlite ()
saveMeme upper lower filepath =
  L.execute "INSERT INTO memes (upper_text, lower_text, image_filepath) \
            \VALUES (?, ?, ?)" (upper, lower, filepath)
