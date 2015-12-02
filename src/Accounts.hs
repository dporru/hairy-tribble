{-# LANGUAGE TemplateHaskell #-}
module Accounts
  (
    Account(Account)
  , Accounts
  , userAccount, AccountStatus(New, Existing)
  , accountsOptionType

  -- For CLI
  , fromString
  ) where


import           Common

import qualified Data.Map         as Map
import qualified Data.SafeCopy    as SC
import qualified Data.TCache      as T
import qualified Data.TCache.Defs as T
import qualified Data.Text        as Text
import           Text.Read    (readEither)
import qualified System.Console.Argument as CP


data Account
  = Account String
  deriving (Generic, Typeable, Eq, Ord, Show)
SC.deriveSafeCopy 0 'SC.base ''Account

type Accounts
  = Map.Map UserID Account

type UserID
  = Text

type UserName
  = Text

data AccountStatus
  = New
  | Existing

userAccount :: Accounts -> (UserID, UserName) -> (Account, AccountStatus)
userAccount m (uid, uName) = case Map.lookup uid m of
  Just a  -> (a, Existing)
  Nothing -> (newAccount,New)
 where
  newAccount = Account $ Text.unpack uName ++ "-" ++ Text.unpack uid

fromString :: String -> Account
fromString = Account

accountsOptionType :: CP.Type Accounts
accountsOptionType = CP.Type
  {
    CP.name         = "accounts"
  , CP.defaultValue = Nothing
  , CP.parser       = (Map.fromList . map (\ (u, a) -> (Text.pack u, Account a)) <$>) . readEither
  }
