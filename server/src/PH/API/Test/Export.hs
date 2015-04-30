{-# LANGUAGE LambdaCase #-}
module PH.API.Test.Export where

import           Common
import qualified PH.API.Test.Export.PDF as PDF
import qualified PH.DB                  as DB
import           PH.Types

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.TCache           as T
import qualified Data.TCache.ID        as ID
import qualified Data.Text             as Text
import qualified Rest                  as R
import           Rest (Void)
import qualified Rest.Handler          as R
import qualified Rest.Resource         as R


resource :: R.Resource
  (ReaderT (ID.Ref Test) IO)
  (ReaderT Format (ReaderT (ID.Ref Test) IO))
  Format
  Void
  Void
resource = R.mkResourceReader
  {
    R.name   = "export"
  , R.schema = R.noListing $ R.named [("format",R.singleBy readFormat)]
  , R.get    = Just get
  }

get :: R.Handler (ReaderT Format (ReaderT (ID.Ref Test) IO))
get = R.mkHandler R.fileO $ \ _env -> ExceptT $
  ReaderT $ \ format ->
  ReaderT $ \ testRef -> do
    test <- DB.run $ ID.deref testRef
    case format of
      Unknown f -> return . Left . R.ParamError $ R.UnsupportedFormat f
      PDF       -> do
        PDF.export WithAnswers test >>= \case
          Left e  -> return . Left . R.OutputError . R.PrintError . B8.unpack $ e
          Right b -> return $ Right (b,suggestedFileName test format)

data Format
  = PDF
  | Unknown String

readFormat :: String -> Format
readFormat "pdf" = PDF
readFormat s     = Unknown s

suggestedFileName :: Test -> Format -> String
suggestedFileName test PDF         = Text.unpack (name test) ++ ".pdf"
suggestedFileName _    (Unknown s) = ""
