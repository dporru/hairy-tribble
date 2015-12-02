{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server where


import           Accounts
  ( Account
  , Accounts
  , userAccount
  , AccountStatus(New, Existing)
  )
import           Common
import qualified PH.DB as DB
import qualified Image
import qualified OAuth2
import           PH.API                     (M, api)
import           PH.Types.Server            (Config, serverHost, stores)
import qualified Session

import qualified Data.Map         as Map
import qualified Happstack.Server as H
import           Rest.Driver.Happstack      (apiToHandler)
import           Rest.Driver.Happstack.Docs (apiDocsHandler)
import           Rest.Driver.Perform        (Rest)
import           System.Directory           (renameFile)
import           System.Random              (newStdGen,randomRs)


serve :: String -> String -> Config -> Accounts -> IO ()
serve clientID clientSecret config accounts = Session.withServerSession $ \ state -> do
  let redirect = view serverHost config ++ "login"
      oauth2 = OAuth2.specifyGoogleKey clientID clientSecret redirect
      settings :: (MonadIO m) => Session.ServerSessionSettings Account m
      settings = Session.ServerSessionSettings
        { Session.oauth2ClientKey = oauth2
        , Session.newSessionData = \ o -> do
            let (a,s) = userAccount accounts $ (OAuth2.id &&& OAuth2.name) o
            case s of
              Existing -> return ()
              New      -> do
                store <- liftIO $ DB.initialise a
                liftIO . putStrLn $ "Adding new account: " ++ show a
                liftIO $ modifyMVar_ (view stores config)
                  (return . Map.insert a store)
            return $ Just a
        , Session.redirectPolicy = Session.Unauthorized
        }
  H.simpleHTTP H.nullConf .
    Session.runServerSessionT settings state .
    flip runReaderT config .
    msum $
      [ H.dir "api" apiHandle
      , H.dir "docs" docsHandle
      , H.dir "uploaded" Image.handler
      , H.serveDirectory H.DisableBrowsing ["index.html"] "./client/"
      , H.serveDirectory H.DisableBrowsing [] "./rest-gen-files/Docs/"
      , H.dir "login" login
      , H.dir "logout" logout
      ]

apiHandle :: M H.Response
apiHandle = do
  (_user,account) <- Session.getSession
  H.decodeBody $ H.defaultBodyPolicy "/tmp/" 1048576 1048576 4096
  config <- ask
  response <- apiToHandler $ api config
  Session.putSession account
  return response

docsHandle :: M H.Response
docsHandle = do
  config <- ask
  apiDocsHandler "/docs/" "rest-gen-files/Docs/" $ api config

deriving instance (Rest m) => Rest (Session.ServerSessionT s m)

login :: M H.Response
login = do
  (_user,account) <- Session.getSession
  Session.putSession account
  host <- view serverHost
  H.seeOther host $ H.toResponse ()

logout :: M H.Response
logout = do
  Session.expireSession
  return . H.toResponse $ ("Logged out" :: String)
