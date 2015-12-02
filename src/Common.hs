{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Common
  (
    (<$>),msum,(<<<),(>>>),(<=<),void
  , (&&&)
  , Identity(Identity)
  , MonadIO,liftIO,lift
  , throwError,ExceptT(ExceptT)
  , MonadReader,ReaderT(ReaderT),runReaderT,ask
  , MonadState,StateT(StateT),evalStateT,get,put,modify
  , for,for_
  , STM,atomically
  , MVar,readMVar,modifyMVar_,newMVar
  , Text,utf8ByteString
  , ToJSON,toJSON,FromJSON,parseJSON
  , gtoJson,gparseJson
  , JSONSchema,schema,gSchema
  , Generic
  , Typeable
  , mempty,(<>),mconcat
  , UTCTime,getCurrentTime
  , Lens,Lens',view,set,over,overM,makeLenses
  , ExportMode(OnlyQuestions,WithAnswers)
  ) where


import           Control.Applicative        ((<$>))
import           Control.Arrow              ((<<<),(>>>),(&&&))
import           Control.Concurrent         (MVar,readMVar,modifyMVar_,newMVar)
import           Control.Concurrent.STM     (STM,atomically)
import           Control.Lens               (Lens,Lens',view,set,over)
import           Control.Lens.Monadic       (overM)
import           Control.Lens.TH            (makeLenses)
import           Control.Monad              (msum,(<=<))
import           Control.Monad.Except       (throwError,ExceptT(ExceptT))
import           Control.Monad.Identity     (Identity(Identity))
import           Control.Monad.IO.Class     (MonadIO,liftIO)
import           Control.Monad.Reader       (MonadReader,ask)
import           Control.Monad.State        (MonadState,get,put,modify)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT(ReaderT),runReaderT)
import           Control.Monad.Trans.State  (StateT(StateT),evalStateT)
import           Data.Aeson                 (ToJSON,toJSON,FromJSON,parseJSON)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as B
import           Data.Foldable              (for_)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Time.Clock            (UTCTime,getCurrentTime)
import           Data.Functor               (void)
import           Data.JSON.Schema           (JSONSchema,schema,gSchema)
import           Data.Monoid                (mempty,(<>),mconcat)
import           Data.Text                  (Text)
import           Data.Traversable           (for)
import           Data.Typeable              (Typeable)
import           Generics.Generic.Aeson     (gtoJson,gparseJson)
import           GHC.Generics               (Generic)

import           Data.Aeson       (encode,eitherDecode)
import qualified Data.TCache.Defs as T

data ExportMode
  = OnlyQuestions
  | WithAnswers

utf8ByteString :: String -> ByteString
utf8ByteString = B.toLazyByteString . Text.encodeUtf8Builder . Text.pack
