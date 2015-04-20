module ID
  (
    WithID(WithID)
  , SupplyT
  , new
  ) where

import           Common

import           Control.Concurrent.Supply (Supply,newSupply,freshId)
import           Control.Monad.Trans.State.Strict (StateT,state)


data WithID a
  = WithID ID a
  deriving (Generic,Typeable)
instance (FromJSON a) => FromJSON (WithID a) where
  parseJSON = gparseJson
instance (ToJSON a) => ToJSON (WithID a) where
  toJSON = gtoJson

type ID
  = Int

type SupplyT
  = StateT Supply

new :: (Monad m,Functor m) => a -> SupplyT m (WithID a)
new a = flip WithID a <$> state freshId
