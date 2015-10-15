{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main
  (
    main
  ) where


import           Common
import           PH.API                     (M,api)
import qualified PH.DB            as DB

import qualified Session

import qualified Happstack.Server as H
import           Rest.Driver.Happstack      (apiToHandler)
import           Rest.Driver.Happstack.Docs (apiDocsHandler)
import           Rest.Driver.Perform        (Rest)
import           System.Directory           (renameFile)
import           System.Random              (newStdGen,randomRs)


serverBaseURI :: String
serverBaseURI = "http://localhost:8000/"

main :: IO ()
main = do
  DB.initialise
  Session.withServerSession $ \ oauth2 state -> H.simpleHTTP H.nullConf . Session.runServerSessionT oauth2 state . msum $
    [
      H.dir "api" apiHandle
    , H.dir "docs" docsHandle
    , H.dir "uploaded" imageHandle
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

imageHandle :: M H.Response
imageHandle = do
  H.decodeBody $ H.defaultBodyPolicy "./uploaded/" (10*1048576) 1024 1024
  msum [upload,view]
 where
  view = H.serveDirectory H.DisableBrowsing [] "./uploaded/"
  upload = do
    H.method H.POST
    (tempFileName,clientFileName,_contentType) <- H.lookFile "fileToUpload"
    fileName <- (++) "uploaded/" <$> liftIO randomName
    liftIO $ renameFile tempFileName fileName
    return $ H.redirect 201 (serverBaseURI ++ fileName) . H.toResponse $ "File created at " ++ show fileName
  randomName = take 16 . randomRs ('a','z') <$> newStdGen

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
