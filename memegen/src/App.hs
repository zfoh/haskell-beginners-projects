{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module App where

import           Control.Lens (makeLenses)
import           Control.Monad.State (get)
import qualified Snap as S
import qualified Snap.Snaplet.SqliteSimple as L

data AppState = AppState
    { _db :: S.Snaplet L.Sqlite
    }

makeLenses ''AppState

instance L.HasSqlite (S.Handler b AppState) where
    getSqliteState = S.with db get
