{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Session
  (
    ServerSessionT(..)
  , withServerSession
  , runServerSessionT
  , MonadServerSession
  , getSession
  , putSession
  , expireSession
  ) where


import           Control.Applicative        (Alternative,optional)
import           Control.Monad              (MonadPlus)
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.IO.Class     (MonadIO,liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT,runReaderT,ask)
import           Control.Monad.Trans.State  (StateT,evalStateT,get,put)
import qualified Data.Acid.Local           as Acid
import qualified Data.ByteString.Char8     as BS8
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.SafeCopy             as SC
import qualified Data.Text                 as Text
import           Data.Typeable              (Typeable)
import qualified Happstack.Server          as H
import qualified Happstack.Server.Cookie   as H
import qualified Happstack.Server.RqData   as H
import qualified Happstack.Server.Response as H
import qualified OAuth2
import           Web.PathPieces             (toPathPiece)
import           Web.ServerSession.Backend.Acid (AcidStorage(..),emptyState)
import qualified Web.ServerSession.Core    as S


newtype ServerSessionT sessionData m a
  = ServerSessionT
    {
      unServerSessionT :: ReaderT (S.State (AcidStorage (SD sessionData)),OAuth2.ClientKey) (StateT (SessionStatus sessionData) m) a
    }
    deriving (Functor,Applicative,Alternative,Monad,MonadPlus,MonadIO,MonadFix,H.HasRqData,H.FilterMonad r,H.WebMonad r,H.ServerMonad)

data SessionStatus sessionData
  = Unread
  | Existing
      OAuth2.User
      (Maybe sessionData)
      (S.SaveSessionToken (AcidStorage (SD sessionData)))

class MonadServerSession m where
  type SessionData m
  getSession    :: m (OAuth2.User,Maybe (SessionData m)) -- ^ get the current @sessionData@
  putSession    :: Maybe (SessionData m) -> m ()         -- ^ set the @sessionData@
  expireSession :: m ()                                  -- ^ expire the session (deletes the cookie)

instance
  ( H.HasRqData m
  , H.FilterMonad H.Response m
  , H.WebMonad H.Response m
  , MonadIO m
  , MonadPlus m
  , SC.SafeCopy sessionData
  , Eq sessionData
  , Show sessionData
  , Typeable sessionData
  ) => MonadServerSession (ServerSessionT sessionData m) where
  type SessionData (ServerSessionT sessionData m) = sessionData
  
  getSession = ServerSessionT $ do
    lift get >>= \case
      Existing user ms _ -> return (user,ms)
      Unread            -> do
        (state,oauth2) <- ask
        maybeCookie <- optional $ BS8.pack <$> H.lookCookieValue "serversession"
        maybeNewUser <- case maybeCookie of
          Just _  -> do
            -- Cookie is set, just continue.
            return Nothing
          Nothing -> do
            -- There is no session cookie set. Maybe the user has just logged in?
            maybeCode <- optional $ LBS.toStrict <$> H.lookBS "code"
            case maybeCode of
              Nothing -> do
                -- No, there is no login code set. We redirect the user to the login page.
                H.finishWith =<< H.seeOther (OAuth2.loginRedirect oauth2) (H.toResponse ("" :: String))
              Just code -> do
                -- Yes, there is a login code. Try to log in.
                liftIO (OAuth2.login oauth2 code) >>= \case
                  Left errorString -> H.finishWith =<< H.internalServerError (H.toResponse errorString)
                  Right user       -> return $ Just user
        (sd,saveSessionToken) <- liftIO $ S.loadSession state maybeCookie
        (user,ms) <- case sd of
          EmptySession -> case maybeNewUser of
            Just user -> return (user,Nothing)
            Nothing   -> do
              H.finishWith =<< H.internalServerError (H.toResponse ("Session: session cookie is set, but the session is empty" :: String))
          SD user s    -> return (user,Just s)
        lift . put $ Existing user ms saveSessionToken
        return (user,ms)
  
  putSession ms = (getSession >>) . ServerSessionT $ do
    (state,_) <- ask
    Existing user _ saveSessionToken <- lift get
    newSessionData <- case ms of
      Nothing -> return $ EmptySession
      Just s  -> return $ SD user s
    liftIO (S.saveSession state saveSessionToken newSessionData) >>= \case
      Nothing      -> H.expireCookie "serversession"
      Just session -> do
        let cookie = H.mkCookie "serversession" $ Text.unpack . toPathPiece $ S.sessionKey session
        let lifetime = maybe H.Session H.Expires $ S.cookieExpires state session
        H.addCookie lifetime cookie
  
  expireSession = H.expireCookie "serversession"

runServerSessionT ::
  ( Monad m
  ) => OAuth2.ClientKey -> S.State (AcidStorage (SD sessionData)) -> ServerSessionT sessionData m a -> m a
runServerSessionT oauth2 state (ServerSessionT h) = flip evalStateT Unread . flip runReaderT (state,oauth2) $ h

withServerSession ::
  ( MonadIO m
  , SC.SafeCopy sessionData
  , Eq sessionData
  , Show sessionData
  , Typeable sessionData
  ) => (S.State (AcidStorage (SD sessionData)) -> m ()) -> m ()
withServerSession h = do
  storage <- liftIO $ AcidStorage <$> Acid.openLocalState emptyState
  state <- S.createState storage
  h state
  liftIO . Acid.createCheckpointAndClose . acidState $ storage

data SD s
  = EmptySession
  | SD OAuth2.User s
  deriving (Eq,Show)
SC.deriveSafeCopy 0 'SC.base ''SD

instance (Eq s,Show s,Typeable s) => S.IsSessionData (SD s) where
  type Decomposed (SD s) = SD s
  emptySession = EmptySession
  decomposeSession _ s = S.DecomposedSession       
    { S.dsAuthId          = Nothing
    , S.dsForceInvalidate = S.DoNotForceInvalidate
    , S.dsDecomposed      = s
    }
  recomposeSession _ _ s = s
  isSameDecomposed _ = (==)
  isDecomposedEmpty _ EmptySession  = True
  isDecomposedEmpty _ (SD _ _)      = False
