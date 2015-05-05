{-# LANGUAGE LambdaCase #-}
module PH.API.Test.Export where

import           Common
import qualified PH.API.Test.Export.LaTeX    as LaTeX
import qualified PH.API.Test.Export.Markdown as Markdown
import qualified PH.API.Test.Export.Pandoc   as Pandoc
import qualified PH.API.Test.Export.PDF      as PDF
import qualified PH.DB                       as DB
import           PH.Types

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.TCache                as T
import qualified Data.TCache.ID             as ID
import qualified Data.Text                  as Text
import qualified Rest                       as R
import           Rest (Void)
import qualified Rest.Dictionary.Types      as R
import qualified Rest.Handler               as R
import qualified Rest.Resource              as R


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
get = R.mkHandler dict $ \ env -> ExceptT $
  ReaderT $ \ format ->
  ReaderT $ \ testRef -> do
    test <- DB.run $ ID.deref testRef
    let mode = R.param env
    case format of
      Unknown f -> return . Left . R.ParamError $ R.UnsupportedFormat f
      _         -> Pandoc.build mode test >>= export >>= \case
        Left e  -> return . Left . R.OutputError . R.PrintError . B8.unpack $ e
        Right b -> return $ Right (b,suggestedFileName test format)
       where
        export = case format of
          PDF      -> PDF.export
          LaTeX    -> LaTeX.export
          Markdown -> Markdown.export
          Show     -> Pandoc.export
 where
  dict = R.mkPar modePar . R.fileO
  modePar = R.Param ["withAnswers"] p where
    p [Nothing] = Right OnlyQuestions
    p [Just ""] = Right WithAnswers
    p [Just x]  = Left . R.ParseError $ "Unexpected query parameter: " ++ x

data Format
  = PDF
  | LaTeX
  | Markdown
  | Show
  | Unknown String

readFormat :: String -> Format
readFormat "pdf"      = PDF
readFormat "latex"    = LaTeX
readFormat "markdown" = Markdown
readFormat "show"     = Show
readFormat s          = Unknown s

suggestedFileName :: Test -> Format -> String
suggestedFileName test PDF         = Text.unpack (name test) ++ ".pdf"
suggestedFileName test LaTeX       = Text.unpack (name test) ++ ".latex"
suggestedFileName test Markdown    = Text.unpack (name test) ++ ".md"
suggestedFileName test Show        = Text.unpack (name test) ++ ".text"
suggestedFileName _    (Unknown s) = ""
