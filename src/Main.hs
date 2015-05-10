module Main where


import           Common
import           PH.API                     (api)
import qualified PH.DB            as DB

import qualified Happstack.Server as H
import           Rest.Driver.Happstack      (apiToHandler')
import           Rest.Driver.Happstack.Docs (apiDocsHandler)


main :: IO ()
main = do
  DB.initialiseIndices
  H.simpleHTTP H.nullConf . msum $
    [
      H.dir "api" apiHandle
    , H.dir "docs" docsHandle
    , H.serveDirectory H.DisableBrowsing ["index.html"] "./client/"
    , H.serveDirectory H.DisableBrowsing [] "./rest-gen-files/Docs/"
    ]

apiHandle :: H.ServerPartT IO H.Response
apiHandle = do
  H.decodeBody (H.defaultBodyPolicy "/tmp/" 1048576 1048576 4096)
  apiToHandler' liftIO api

docsHandle :: H.ServerPartT IO H.Response
docsHandle = apiDocsHandler "/docs/" "rest-gen-files/Docs/" api
