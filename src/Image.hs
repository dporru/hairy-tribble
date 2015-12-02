module Image where


import           Common
import           PH.API                     (M)
import           PH.Types.Server            (serverHost)

import qualified Data.Aeson       as Aeson
import qualified Data.Text        as Text
import qualified Happstack.Server as H
import           System.Directory           (renameFile)
import           System.Random              (newStdGen,randomRs)


handler :: M H.Response
handler = do
  H.decodeBody $ H.defaultBodyPolicy "./uploaded/" (10*1048576) 1024 1024
  msum [upload, browse]
 where
  browse = H.serveDirectory H.DisableBrowsing [] "./uploaded/"
  upload = do
    H.method H.POST
    (tempFileName,clientFileName,_contentType) <- H.lookFile "fileToUpload"
    fileName <- (++) "uploaded/" <$> liftIO randomName
    liftIO $ renameFile tempFileName fileName
    host <- view serverHost <$> ask
    return $ H.redirect 201 (host ++ fileName) $ response fileName
  randomName = take 16 . randomRs ('a','z') <$> newStdGen

response :: FilePath -> H.Response
response f = H.toResponse $ Aeson.object [("message","uploadSuccess"),("file",Aeson.String $ Text.pack f)]

instance H.ToMessage Aeson.Value where
  toContentType _ = "application/json"
  toMessage       = Aeson.encode
