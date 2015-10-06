{-# LANGUAGE LambdaCase #-}
module PH.API.Test.Export.Pandoc
  (
    build
  , export
  ) where

import           Common
import qualified PH.DB                 as DB
import           PH.Types

import           Data.ByteString.Lazy (ByteString)
import qualified Data.TCache.ID          as ID
import qualified Data.Text               as Text
import qualified Text.Pandoc             as P
import qualified Text.Pandoc.Builder     as P


export:: P.Pandoc -> IO (Either ByteString ByteString)
export = return . Right . utf8ByteString . P.writeNative P.def

build :: ExportMode -> Decorated Test -> IO P.Pandoc
build mode test = do
  qts <- DB.run $ for (view (undecorated . elements) test) $ \case
    TestQuestion i -> return . Right . view (ID.object . undecorated) =<< ID.deref i
    TestText t     -> return $ Left t
  return $ renderTest mode (view (undecorated . name) test) qts

renderTest :: ExportMode -> Text -> [Either Text Question] -> P.Pandoc
renderTest mode name qs = P.setTitle (textP name) . P.doc $
  P.orderedList (map renderQuestion qs)
 where
  renderQuestion (Left t ) = P.para $ textP t
  renderQuestion (Right q) = P.para (textP $ view question q) <> case view answer q of
    Open a                -> case mode of
      OnlyQuestions       -> mempty
      WithAnswers         -> P.para $ P.strong (textP a)
    m@(MultipleChoice {}) -> P.orderedListWith (1,P.UpperAlpha,P.OneParen)
      $ map renderChoice answers
     where
      answers = ((True,view correct m) : map ((,) False) (view incorrect m))
        `orderBy` view order m
      renderChoice (corr,t) = P.para $ case mode of
        OnlyQuestions -> textP t
        WithAnswers   -> (if corr then P.strong else P.strikeout) $ textP t

orderBy :: [a] -> [Int] -> [a]
orderBy = map . (!!)

textP :: Text -> P.Inlines
textP = P.str . Text.unpack
