{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server where


import           Common
import qualified Image
import qualified OAuth2
import           PH.API                     (M,api)
import qualified Session

import qualified Happstack.Server as H
import           Rest.Driver.Happstack      (apiToHandler)
import           Rest.Driver.Happstack.Docs (apiDocsHandler)
import           Rest.Driver.Perform        (Rest)
import           System.Directory           (renameFile)
import           System.Random              (newStdGen,randomRs)


serve :: String -> String -> String -> IO ()
serve clientID clientSecret serverHost = Session.withServerSession $ \ state -> do
  let redirect = serverHost ++ "login"
      oauth2 = OAuth2.specifyGoogleKey clientID clientSecret redirect
  H.simpleHTTP H.nullConf .
    Session.runServerSessionT oauth2 state .
    flip runReaderT serverHost .
    msum $
      [
        H.dir "api" apiHandle
      , H.dir "docs" docsHandle
      , H.dir "uploaded" Image.handler
      , H.serveDirectory H.DisableBrowsing ["index.html"] "./client/"
      , H.serveDirectory H.DisableBrowsing [] "./rest-gen-files/Docs/"
      , H.dir "logout" logout
      , H.dir "login" login
      ]

apiHandle :: M H.Response
apiHandle = do
  (user,maybeCounter) <- lift Session.getSession
  H.decodeBody $ H.defaultBodyPolicy "/tmp/" 1048576 1048576 4096
  response <- apiToHandler api
  lift . Session.putSession . Just $ maybe 0 succ maybeCounter
  return response

docsHandle :: M H.Response
docsHandle = apiDocsHandler "/docs/" "rest-gen-files/Docs/" api


deriving instance (Rest m) => Rest (Session.ServerSessionT s m)

logout :: M H.Response
logout = do
  lift . Session.putSession $ Nothing
  lift Session.expireSession
  return . H.toResponse $ ("Logged out" :: String)

login :: M H.Response
login = do
  (user,mi) <- lift Session.getSession
  lift . Session.putSession . Just $ maybe 0 succ mi
  host <- ask
  H.seeOther host $ H.toResponse ()
