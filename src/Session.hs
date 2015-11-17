{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Session
  (
    ServerSessionT(..)
  , ServerSessionSettings(..)
  , RedirectPolicy(..)
  , withServerSession
  , runServerSessionT
  , MonadServerSession,SessionData
  , getSession
  , getSessionData
  , putSession
  , expireSession
  ) where


import           Control.Applicative        (Alternative,optional)
import           Control.Monad              (MonadPlus,(<=<))
import           Control.Monad.Catch        (MonadMask,bracket)
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.IO.Class     (MonadIO,liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT)
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
import           Network.URI                (URI)
import qualified OAuth2
import           Web.PathPieces             (toPathPiece)
import           Web.ServerSession.Backend.Acid (AcidStorage(..),emptyState)
import qualified Web.ServerSession.Core    as S


newtype ServerSessionT sessionData m a
  = ServerSessionT
    {
      unServerSessionT :: ReaderT
        (S.State (AcidStorage (SD sessionData)), ServerSessionSettings sessionData)
        (StateT (SessionStatus sessionData) m)
        a
    }
    deriving (Functor,Applicative,Alternative,Monad,MonadPlus,MonadIO,MonadFix,H.HasRqData,H.FilterMonad r,H.WebMonad r,H.ServerMonad)

data ServerSessionSettings sessionData
  = ServerSessionSettings
    {
      oauth2ClientKey :: OAuth2.ClientKey
    , newSessionData  :: OAuth2.User -> Maybe sessionData
    , redirectPolicy  :: RedirectPolicy
    }

data RedirectPolicy
  = Redirect
  | Unauthorized

redirect ::
  ( H.FilterMonad H.Response m
  , H.WebMonad H.Response m
  ) => URI -> ReaderT (s,ServerSessionSettings sessionData) m ()
redirect uri = do
  let u = show uri
  (_state,settings) <- ask
  case redirectPolicy settings of
    Redirect     -> H.finishWith =<< H.seeOther u (H.toResponse ("" :: String))
    Unauthorized -> H.finishWith =<< H.unauthorized (H.toResponse u)

data SessionStatus sessionData
  = Unread
  | Existing
      OAuth2.User
      sessionData
      (S.SaveSessionToken (AcidStorage (SD sessionData)))

class (Monad m) => MonadServerSession m where
  type SessionData m
  getSession    :: m (OAuth2.User,SessionData m) -- ^ Get the current user and session data.
  putSession    :: SessionData m -> m ()         -- ^ Set the session data.
  expireSession :: m ()                          -- ^ Expire the session and delete the cookie.

getSessionData :: (MonadServerSession m) => m (SessionData m)
getSessionData = snd <$> getSession

instance forall m sessionData.
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
        (state,settings) <- ask
        let redirectToLogin :: ReaderT (s, ServerSessionSettings sessionData)
              (StateT (SessionStatus sessionData) m) a
            redirectToLogin = do
              redirect $ OAuth2.loginRedirect $ oauth2ClientKey settings
              error "Session: should not be reached due to redirect."
        maybeCookie <- optional $ BS8.pack <$> H.lookCookieValue "serversession"
        maybeNewUser <- case maybeCookie of
          -- Cookie is set, just continue.
          Just _  -> return Nothing
          Nothing -> do
            -- There is no session cookie set. Maybe the user has just logged in?
            maybeCode <- optional $ LBS.toStrict <$> H.lookBS "code"
            case maybeCode of
              -- No, there is no login code set. We redirect the user to the login page.
              Nothing -> redirectToLogin
              -- Yes, there is a login code. Try to log in.
              Just code -> liftIO (OAuth2.login (oauth2ClientKey settings) code) >>= \case
                Left errorString -> H.finishWith =<< H.internalServerError
                  (H.toResponse $ "Session: error logging in:\n" ++ errorString)
                Right user       -> return $ Just user
        (sd,saveSessionToken) <- liftIO $ S.loadSession state maybeCookie
        (user,s) <- case sd of
          EmptySession -> case maybeNewUser of
            -- A session cookie is set, but the session is empty.
            -- Maybe the session store was emptied and the user has a stale session.
            Nothing   -> redirectToLogin
            Just user -> case newSessionData settings user of
              Just s  -> return (user,s)
              Nothing -> do
                liftIO . putStrLn $ "User without account: " ++ show user
                (H.finishWith =<<) . H.forbidden . H.toResponse $ 
                  "There is no account registered for you. Sorry!\nYour details:\n"
                    ++ show user
          SD user s    -> return (user,s)
        lift . put $ Existing user s saveSessionToken
        return (user,s)
  
  putSession s = (getSession >>) . ServerSessionT $ do
    (state,_) <- ask
    Existing user _ saveSessionToken <- lift get
    let newSessionData = SD user s
    liftIO (S.saveSession state saveSessionToken newSessionData) >>= \case
      Nothing      -> H.expireCookie "serversession"
      Just session -> do
        let cookie = H.mkCookie "serversession" $ Text.unpack . toPathPiece $ S.sessionKey session
        let lifetime = maybe H.Session H.Expires $ S.cookieExpires state session
        H.addCookie lifetime cookie
  
  expireSession = H.expireCookie "serversession"

runServerSessionT ::
  ( Monad m
  ) => ServerSessionSettings sessionData
    -> S.State (AcidStorage (SD sessionData))
    -> ServerSessionT sessionData m a
    -> m a
runServerSessionT settings state (ServerSessionT h) = flip evalStateT Unread . flip runReaderT (state,settings) $ h

withServerSession ::
  ( MonadIO m
  , MonadMask m
  , SC.SafeCopy sessionData
  , Eq sessionData
  , Show sessionData
  , Typeable sessionData
  ) => (S.State (AcidStorage (SD sessionData)) -> m ()) -> m ()
withServerSession h = bracket
  (liftIO $ AcidStorage <$> Acid.openLocalState emptyState)
  (liftIO . Acid.createCheckpointAndClose . acidState)
  (h <=< S.createState)

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

instance (MonadServerSession m) => MonadServerSession (ReaderT r m) where
  type SessionData (ReaderT r m) = SessionData m
  getSession    = lift getSession
  putSession    = lift . putSession
  expireSession = lift expireSession

instance (MonadServerSession m) => MonadServerSession (ExceptT e m) where
  type SessionData (ExceptT e m) = SessionData m
  getSession    = lift getSession
  putSession    = lift . putSession
  expireSession = lift expireSession
