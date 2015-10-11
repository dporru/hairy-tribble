{-# LANGUAGE TemplateHaskell #-}
module OAuth2
  (
    loginRedirect
  , login
  , promptGoogleKey
  , User(..)
  , ClientKey
  ) where

import qualified Network.OAuth.OAuth2  as O

import           Control.Applicative        ((<$>))
import           Control.Monad.Trans.Except (ExceptT(ExceptT),runExceptT,withExceptT)
import           Data.Aeson.TH              (defaultOptions,deriveJSON)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.SafeCopy              as SC
import           Data.Text                  (Text)
import qualified Network.HTTP.Conduit       as H
import           Network.URI                (URI,parseURI)
import           Prelude                    hiding (id)
import qualified Prelude                    as P (id)


type ClientKey
  = O.OAuth2

promptGoogleKey :: IO ClientKey
promptGoogleKey = do
  putStrLn "OAuth2 client id:"
  clientId <- fmap BS8.pack getLine
  putStrLn "OAuth2 client secret:"
  clientSecret <- fmap BS8.pack getLine
  return $ O.OAuth2
    { O.oauthClientId = clientId
    , O.oauthClientSecret = clientSecret
    , O.oauthCallback = Just "http://localhost:8000/"
    , O.oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
    , O.oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
    }

data Token = Token
  { issued_to      :: Text
  , audience       :: Text
  , user_id        :: Text
  , scope          :: Text
  , expires_in     :: Integer
  , email          :: Text
  , verified_email :: Bool
  , access_type    :: Text
  }
  deriving (Show)
deriveJSON defaultOptions ''Token

data User = User
  { id          :: Text
  , name        :: Text
  , given_name  :: Text
  , family_name :: Text
  , picture     :: Text
  , locale      :: Text
  }
  deriving (Eq,Show)
deriveJSON defaultOptions ''User
SC.deriveSafeCopy 0 'SC.base ''User


loginRedirect :: ClientKey -> URI
loginRedirect key = uri $ O.authorizationUrl key `O.appendQueryParam` googleScopeBoth where
  -- Gain read-only access to basic profile information and email address
  googleScopeBoth :: O.QueryParams
  googleScopeBoth = [("scope", "https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile")]
  
  uri :: BS8.ByteString -> URI
  uri = maybe e P.id . parseURI . BS8.unpack where
    e = error "OAuth2.loginRedirect: invalid URI"

login :: ClientKey -> BS8.ByteString -> IO (Either String User)
login key code = do
  mgr <- H.newManager H.tlsManagerSettings
  runExceptT . withExceptT BSL8.unpack $ do
    token <- ExceptT $ O.fetchAccessToken mgr key code
    ExceptT $ validateToken mgr token
    ExceptT $ userinfo mgr token

validateToken :: H.Manager -> O.AccessToken -> IO (O.OAuth2Result Token)
validateToken mgr token = O.parseResponseJSON <$> O.authGetBS' mgr token url where
  url = "https://www.googleapis.com/oauth2/v1/tokeninfo"

userinfo :: H.Manager -> O.AccessToken -> IO (O.OAuth2Result User)
userinfo mgr token = O.authGetJSON mgr token url where
  url = "https://www.googleapis.com/oauth2/v2/userinfo"
