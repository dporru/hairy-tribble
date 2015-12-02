{-# LANGUAGE LambdaCase #-}
module PH.API.Test.Export where

import           Accounts (Account)
import           Common
import qualified PH.API.Test.Export.LaTeX    as LaTeX
import qualified PH.API.Test.Export.Markdown as Markdown
import qualified PH.API.Test.Export.Pandoc   as Pandoc
import qualified PH.API.Test.Export.PDF      as PDF
import qualified PH.API.Test.Export.Word     as Word
import qualified PH.DB                       as DB
import           PH.Types
import           PH.Types.Server (Config, stores)
import           Session (MonadServerSession,SessionData,getSessionData)

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map                   as Map
import qualified Data.TCache                as T
import qualified Data.TCache.ID             as ID
import qualified Data.Text                  as Text
import qualified Rest                       as R
import           Rest (Void)
import qualified Rest.Dictionary.Types      as R
import qualified Rest.Handler               as R
import qualified Rest.Resource              as R


resource :: forall m.
  ( MonadServerSession m
  , SessionData m ~ Account
  , MonadIO m
  ) => Config -> R.Resource
  (ReaderT (ID.ID (Decorated Test)) m)
  (ReaderT Format (ReaderT (ID.ID (Decorated Test)) m))
  Format
  Void
  Void
resource config = R.mkResourceReader
  {
    R.name   = "export"
  , R.schema = R.noListing $ R.unnamedSingle readFormat
  , R.get    = Just $ getHandler config
  }

getHandler :: forall m.
  ( MonadServerSession m, SessionData m ~ Account
  , MonadIO m
  ) => Config -> R.Handler (ReaderT Format (ReaderT (ID.ID (Decorated Test)) m))
getHandler config = R.mkHandler dict $ \ env -> ExceptT $
  ReaderT $ \ format ->
  ReaderT $ \ testID -> do
    account <- getSessionData
    s <- liftIO . readMVar $ view stores config
    let Just store = Map.lookup account s
    ID.WithID _ dtest <- DB.run store . DB.withStore $ \ s -> ID.fromID s testID
    let test = view undecorated dtest
    let mode = R.param env
    case format of
      Unknown f -> return . Left . R.ParamError $ R.UnsupportedFormat f
      _         -> Pandoc.build config mode dtest >>= export >>= \case
        Left e  -> return . Left . R.OutputError . R.PrintError . B8.unpack $ e
        Right b -> return $ Right (b,suggestedFileName test mode format,False)
       where
        export = case format of
          PDF      -> PDF.export
          LaTeX    -> LaTeX.export
          Markdown -> Markdown.export
          Show     -> Pandoc.export
          Word     -> Word.export
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
  | Word
  | Unknown String

readFormat :: String -> Format
readFormat "pdf"      = PDF
readFormat "latex"    = LaTeX
readFormat "markdown" = Markdown
readFormat "show"     = Show
readFormat "word"     = Word
readFormat s          = Unknown s

suggestedFileName :: Test -> ExportMode -> Format -> String
suggestedFileName test mode format = prefix ++ title ++ extension where
  prefix = case mode of
    OnlyQuestions -> "Toets."
    WithAnswers   -> "Antwoorden."
  title = Text.unpack $ view name test
  extension = case format of
    PDF       -> ".pdf"
    LaTeX     -> ".latex"
    Markdown  -> ".md"
    Show      -> ".text"
    Word      -> ".docx"
    Unknown s -> ""
