{-# LANGUAGE LambdaCase #-}
module PH.API.Test.Export.Pandoc
  (
    build
  , export
  ) where

import           Accounts (Account)
import           Common
import qualified PH.DB                 as DB
import           PH.Types
import           Session (MonadServerSession,SessionData,getSessionData)

import           Data.ByteString.Lazy (ByteString)
import qualified Data.TCache.ID          as ID
import qualified Data.Text               as Text
import qualified Text.Pandoc             as P
import qualified Text.Pandoc.Builder     as P
import qualified Text.Pandoc.Walk        as P


export :: (Monad m) => P.Pandoc -> m (Either ByteString ByteString)
export = return . Right . utf8ByteString . P.writeNative P.def

build ::
  ( MonadServerSession m
  , SessionData m ~ Account
  , MonadIO m
  ) => ExportMode -> Decorated Test -> m P.Pandoc
build mode test = do
  s <- getSessionData
  qts <- DB.run s . DB.withStore $ \ store -> for (view (undecorated . elements) test) $ \case
    TestQuestion i -> return . Right . view (ID.object . undecorated) =<< ID.deref store i
    TestText t     -> return $ Left t
  return $ renderTest mode (view (undecorated . name) test) qts

renderTest :: ExportMode -> Text -> [Either RichText Question] -> P.Pandoc
renderTest mode name qs = P.setTitle (textP name) . P.doc $
  P.orderedList (map renderQuestion qs)
 where
  renderQuestion :: Either RichText Question -> P.Blocks
  renderQuestion (Left (Pandoc b)) = P.fromList b
  renderQuestion (Right q)         = let Pandoc b = view question q in
    P.fromList b <> case view answer q of
      Open (Pandoc a)       -> case mode of
        OnlyQuestions -> mempty
        WithAnswers   -> P.fromList $ strong a
      m@(MultipleChoice {}) -> P.orderedListWith (1,P.UpperAlpha,P.OneParen)
        $ map renderChoice answers
       where
        answers :: [(Bool,RichText)]
        answers = view choices m `orderBy` view order m
        renderChoice :: (Bool,RichText) -> P.Blocks
        renderChoice (corr,Pandoc t) = case mode of
          OnlyQuestions -> P.fromList t
          WithAnswers   -> P.fromList $ (if corr then strong else strikeout) t

strong :: [P.Block] -> [P.Block]
strong = map $ P.walk $ P.Strong . (: [])

strikeout :: [P.Block] -> [P.Block]
strikeout = map $ P.walk $ P.Strikeout . (: [])

orderBy :: [a] -> AnswerOrder -> [a]
-- orderBy = map . (!!)
orderBy = const

textP :: Text -> P.Inlines
textP = P.str . Text.unpack
