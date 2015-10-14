{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where


import           Common
import           PH.API                     (M,api)
import qualified PH.DB            as DB

import qualified Session

import qualified Happstack.Server as H
import           Rest.Driver.Happstack      (apiToHandler)
import           Rest.Driver.Happstack.Docs (apiDocsHandler)
import           Rest.Driver.Perform        (Rest)


main :: IO ()
main = do
  DB.initialise
  Session.withServerSession $ \ oauth2 state -> H.simpleHTTP H.nullConf . Session.runServerSessionT oauth2 state . msum $
    [
      H.dir "api" apiHandle
    , H.dir "docs" docsHandle
    , H.serveDirectory H.DisableBrowsing ["index.html"] "./client/"
    , H.serveDirectory H.DisableBrowsing [] "./rest-gen-files/Docs/"
    , H.dir "logout" logoutTest
    , loginTest
    ]

apiHandle :: M H.Response
apiHandle = do
  (user,maybeCounter) <- Session.getSession
  H.decodeBody $ H.defaultBodyPolicy "/tmp/" 1048576 1048576 4096
  response <- apiToHandler api
  Session.putSession . Just $ maybe 0 succ maybeCounter
  return response

docsHandle :: M H.Response
docsHandle = apiDocsHandler "/docs/" "rest-gen-files/Docs/" api

deriving instance (Rest m) => Rest (Session.ServerSessionT s m)

logoutTest :: M H.Response
logoutTest = do
  Session.putSession $ Nothing
  Session.expireSession
  return . H.toResponse $ ("Logged out" :: String)

loginTest :: M H.Response
loginTest = do
  (user,mi) <- Session.getSession
  Session.putSession . Just $ maybe 0 succ mi
  return . H.toResponse $ show user ++ "\nCounter: " ++ show mi
