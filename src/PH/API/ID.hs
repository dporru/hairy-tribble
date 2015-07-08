{-# LANGUAGE FlexibleContexts #-}
module PH.API.ID
  (
    Resource
  , resource
  ) where

import           Common
import           PH.Types (Decorated)
import           PH.Types.Dated
import           PH.Types.Labelled

import qualified Data.TCache      as T
import qualified Data.TCache.Defs as T
import qualified Data.TCache.ID   as ID
import qualified Data.TCache.Index.Map as IndexMap

import qualified Rest             as R
import           Rest (Void)
import qualified Rest.Handler     as R
import qualified Rest.Resource    as R

import           Control.Monad.Except   (MonadError,throwError)
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Text        as Text

type Resource o
  = R.Resource IO (ReaderT (ID.Ref (Decorated o)) IO) (ID.Ref (Decorated o)) [Label] Void

resource :: forall o.
  ( T.Serializable (ID.WithID (Decorated o)),T.IResource (ID.WithID (Decorated o))
  , JSONSchema (ID.Ref (Decorated o)),JSONSchema (ID.WithID (Decorated o))
  , JSONSchema (Decorated o),JSONSchema (Labelled o)
  , ToJSON (ID.Ref (Decorated o)),ToJSON (ID.WithID (Decorated o)),ToJSON (Decorated o)
  , FromJSON (Decorated o),FromJSON (Labelled o)
  , Typeable o
  -- Indices:
  , T.IResource (Map.Map (ID.ID (Decorated o)) (Set.Set (ID.Ref (Decorated o))))
  , T.Serializable (Map.Map (ID.ID (Decorated o)) (Set.Set (ID.Ref (Decorated o))))
  , T.Indexable    (Map.Map (ID.ID (Decorated o)) (Set.Set (ID.Ref (Decorated o))))
  , T.Serializable (Map.Map Text.Text             (Set.Set (ID.Ref (Decorated o))))
  ) => String -> Resource o
resource name = R.mkResourceReader
  {
    R.name   = name
  , R.schema = R.withListing [] $ R.named
      [
        ("id"   ,R.singleBy T.getDBRef)
      , ("label",R.listingBy parseLabels)
      ]
  , R.list   = list
  , R.get    = Just get
  , R.update = Just update
  , R.create = Just create
  , R.remove = Just remove
  }
 where
  get = R.mkIdHandler R.jsonO $ \ () -> run . T.readDBRef
  list ls = R.mkListing R.jsonO $ \ range -> do
    xs :: [ID.WithID (Decorated o)] <- run $ mapM ID.deref =<< IndexMap.lookupAll labelsIndex ls
    let current = filter (not . isDeleted . view (ID.object . withoutLabels)) $ xs
    return $ take (R.count range) . drop (R.offset range) $ current
  create = R.mkInputHandler (R.jsonI . R.jsonO) $ \ (o :: Labelled o) -> do
    run . ID.newRef =<< withoutLabels date o
  remove = R.mkIdHandler id $ \ () i -> do
    c <- liftIO getCurrentTime
    found <- run $ overM (ID.refLens . withoutLabels) (deleteDated c) i
    maybeNotFound found
  update = R.mkIdHandler R.jsonI $ \ (x :: Decorated o) (i :: ID.Ref (Decorated o)) -> do
    c <- liftIO getCurrentTime
    found <- run (ID.update i . over withoutLabels (updateDated c) $ x)
    maybeNotFound found
  parseLabels = Text.split f . Text.pack where
    f ',' = True
    f ';' = True
    f _   = False
  
  run :: (MonadIO m) => STM a -> m a
  run = liftIO . T.atomicallySync

maybeNotFound :: (MonadError (R.Reason e) m) => Maybe () -> m ()
maybeNotFound = maybe (throwError R.NotFound) return

labelsIndex :: IndexMap.Field (ID.WithID (Decorated o)) Set.Set Text.Text
labelsIndex = IndexMap.namedFields (view labels . ID._object) "labels"
