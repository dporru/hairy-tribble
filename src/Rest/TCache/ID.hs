{-# LANGUAGE FlexibleContexts #-}
module Rest.TCache.ID
  (
    Resource
  , resource
  ) where

import           PH.Types.Dated

import qualified Data.TCache      as T
import qualified Data.TCache.Defs as T
import qualified Data.TCache.ID   as ID

import qualified Rest             as R
import           Rest (Void)
import qualified Rest.Handler     as R
import qualified Rest.Resource    as R

import           Control.Applicative        ((<$>))
import           Control.Concurrent.STM     (STM)
import           Control.Monad.Except       (throwError)
import           Control.Monad.IO.Class     (MonadIO,liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (ToJSON,FromJSON)
import qualified Data.Map         as Map
import           Data.JSON.Schema           (JSONSchema)
import qualified Data.Set         as Set
import           Data.Typeable              (Typeable)


type Resource o
  = R.Resource IO (ReaderT (ID.Ref (Dated o)) IO) (ID.Ref (Dated o)) () Void

resource :: forall o.
  ( T.Indexable (ID.WithID o),T.PersistIndex (ID.WithID o)
  , T.Serializable (ID.WithID (Dated o)),T.IResource (ID.WithID o)
  , JSONSchema (ID.Ref (Dated o)),JSONSchema (ID.WithID (Dated o)),JSONSchema (Dated o)
  , JSONSchema o
  , ToJSON (ID.Ref (Dated o)),ToJSON (ID.WithID (Dated o)),ToJSON (Dated o),FromJSON o
  , Typeable o
  , T.IResource (Map.Map (ID.ID (Dated o)) (Set.Set (ID.Ref (Dated o))))
  , T.Serializable (Map.Map (ID.ID (Dated o)) (Set.Set (ID.Ref (Dated o))))
  , T.Indexable    (Map.Map (ID.ID (Dated o)) (Set.Set (ID.Ref (Dated o))))
  ) => String -> Resource o
resource name = R.mkResourceReader
  {
    R.name   = name
  , R.schema = R.withListing () $ R.unnamedSingle T.getDBRef
  , R.list   = const list
  , R.get    = Just get
  , R.update = Just update
  , R.create = Just create
  , R.remove = Just remove
  }
 where
  get = R.mkIdHandler R.jsonO $ \ () -> run . ID.maybeDeref
  list = R.mkListing R.jsonO $ \ range ->
    take (R.count range) . drop (R.offset range)
     . filter (not . deleted . ID.object)
    <$> run (ID.listWithID :: STM [ID.WithID (Dated o)])
  create = R.mkInputHandler (R.jsonI . R.jsonO) $ \ (o :: o) -> do
    run . ID.newRef =<< date o
  remove = R.mkIdHandler id $ \ () i -> do
    c <- liftIO getCurrentTime
    run $ ID.update i . deleteDated c =<< ID.deref i
    return ()
  update = R.mkIdHandler R.jsonI $ \ (x :: o) i -> do
    c <- liftIO getCurrentTime
    Dated oldDates _ <- run $ ID.deref i
    maybe (throwError R.NotFound) return =<< run (ID.update i $ updateDated c $ Dated oldDates x)
  
  run :: (MonadIO m) => STM a -> m a
  run = liftIO . T.atomicallySync
