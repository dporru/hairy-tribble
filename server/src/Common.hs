{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Common
  (
    (<$>),msum,(<<<),(>>>),void
  , liftIO,lift
  , throwError,ExceptT(ExceptT)
  , ReaderT(ReaderT)
  , for
  , STM,atomically
  , Text
  , ToJSON,toJSON,FromJSON,parseJSON
  , gtoJson,gparseJson
  , JSONSchema,schema,gSchema
  , Generic
  , Typeable
  , mempty,(<>)
  , Range,takeRange
  , ExportMode(OnlyQuestions,WithAnswers)
  ) where


import Control.Applicative        ((<$>))
import Control.Arrow              ((<<<),(>>>))
import Control.Concurrent.STM     (STM,atomically)
import Control.Monad              (msum)
import Control.Monad.Except       (throwError,ExceptT(ExceptT))
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ReaderT(ReaderT))
import Data.Aeson                 (ToJSON,toJSON,FromJSON,parseJSON)
import Data.Functor               (void)
import Data.JSON.Schema           (JSONSchema,schema,gSchema)
import Data.Monoid                (mempty,(<>))
import Data.Text                  (Text)
import Data.Traversable           (for)
import Data.Typeable              (Typeable)
import Generics.Generic.Aeson     (gtoJson,gparseJson)
import GHC.Generics               (Generic)

import           Data.Aeson       (encode,eitherDecode)
import           Data.TCache.DefaultPersistence ()
import qualified Data.TCache.Defs as T
import           Rest             (Range(..))

instance (FromJSON a,ToJSON a) => T.Serializable a where
  serialize   = encode
  deserialize = either error id . eitherDecode

takeRange :: Range -> [a] -> [a]
takeRange r = take (count r) . drop (offset r)

data ExportMode
  = OnlyQuestions
  | WithAnswers
