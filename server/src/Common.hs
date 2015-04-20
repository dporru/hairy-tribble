module Common
  (
    (<$>),msum
  , liftIO,lift
  , Text
  , ToJSON,toJSON,FromJSON,parseJSON
  , gtoJson,gparseJson
  , JSONSchema,schema,gSchema
  , Generic
  , Typeable
  ) where


import Control.Applicative        ((<$>))
import Control.Monad              (msum)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Class  (lift)
import Data.Aeson                 (ToJSON,toJSON,FromJSON,parseJSON)
import Data.JSON.Schema           (JSONSchema,schema,gSchema)
import Data.Text                  (Text)
import Data.Typeable              (Typeable)
import Generics.Generic.Aeson     (gtoJson,gparseJson)
import GHC.Generics               (Generic)
