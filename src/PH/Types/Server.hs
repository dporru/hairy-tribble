{-# LANGUAGE TemplateHaskell #-}
module PH.Types.Server where

import           Accounts (Account)
import           Common
import qualified PH.DB as DB
import qualified Session

import qualified Happstack.Server   as H

type M
  = ReaderT Config (Session.ServerSessionT Account (H.ServerPartT IO))

type ServerHost
  = String

data Config
  = Config
  { _serverHost :: ServerHost
  , _stores     :: MVar DB.Stores
  }
makeLenses ''Config


