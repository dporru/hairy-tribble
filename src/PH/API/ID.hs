{-# LANGUAGE FlexibleContexts #-}
module PH.API.ID
  (
    Resource
  , resource
  
  , idIndex
  , labelsIndex
  ) where

import           Common
import           PH.Types (Decorated)
import           PH.Types.Dated
import           PH.Types.JSON.Flatten (In,Out,fromIn,toOut)
import           PH.Types.Labelled

import qualified Data.TCache           as T
import qualified Data.TCache.Defs      as T
import qualified Data.TCache.ID        as ID
import qualified Data.TCache.Index     as Index
import qualified Data.TCache.Index.Map as IndexMap

import qualified Rest             as R
import           Rest (Void)
import qualified Rest.Handler     as R
import qualified Rest.Resource    as R

import           Control.Monad.Except   (MonadError,throwError)
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Text        as Text

type Resource m o
  = R.Resource m (ReaderT (ID.Ref (Decorated o)) m) (ID.Ref (Decorated o)) [Label] Void

resource :: forall m o.
  ( MonadIO m
  , T.Serializable (ID.WithID (Decorated o)),T.IResource (ID.WithID (Decorated o))
  , JSONSchema o,JSONSchema (ID.ID (Decorated o)),JSONSchema (ID.Ref (Decorated o))
  , ToJSON (ID.Ref (Decorated o)),ToJSON (ID.ID (Decorated o))
  , ToJSON o
  , FromJSON o
  , Typeable o
  -- Indices:
  , T.IResource    (Index.LabelledIndex (IndexMap.Field (ID.WithID (Decorated o)) Identity (ID.ID (Decorated o))))
  , T.IResource    (Index.LabelledIndex (IndexMap.Field (ID.WithID (Decorated o)) Set.Set  Text                 ))
  , T.PersistIndex (ID.WithID (Decorated o))
  , T.IResource    (Map.Map (ID.ID (Decorated o)) (Set.Set (ID.Ref (Decorated o))))
  , T.Serializable (Map.Map (ID.ID (Decorated o)) (Set.Set (ID.Ref (Decorated o))))
  , T.Indexable    (Map.Map (ID.ID (Decorated o)) (Set.Set (ID.Ref (Decorated o))))
  , T.Serializable (Map.Map Text.Text             (Set.Set (ID.Ref (Decorated o))))
  ) => String -> Resource m o
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
  get = R.mkIdHandler R.jsonO $ \ () -> (fmap toOut <$>) . run . T.readDBRef
  list ls = R.mkListing R.jsonO $ \ range -> map toOut <$> do
    xs :: [ID.WithID (Decorated o)] <- run . (mapM ID.deref =<<) $ case ls of
      [] -> concatMap (Set.toList . snd) <$> IndexMap.listAll idIndex
      _  -> IndexMap.lookupAll labelsIndex ls
    let current = filter (not . isDeleted . view (ID.object . withoutLabels)) $ xs
    return . takeRange range $ current
  create = R.mkInputHandler (R.jsonI . R.jsonO) $ \ (o :: In o) -> do
    run . ID.newRef =<< withoutLabels date (fromIn o)
  remove = R.mkIdHandler id $ \ () i -> do
    c <- liftIO getCurrentTime
    found <- run $ overM (ID.refLens . withoutLabels) (deleteDated c) i
    maybeNotFound found
  update = R.mkIdHandler R.jsonI $ \ (inO :: In o) (i :: ID.Ref (Decorated o)) -> do
    let inputX = fromIn inO
    c <- liftIO getCurrentTime
    found <- run $ do
      originalX <- ID.derefObject i
      let newDates = set modificationDate c . view (withoutLabels . dates) $ originalX
      ID.update i
        $ Labelled (view labels inputX)
        $ Dated newDates
        $ view withoutLabels inputX
    maybeNotFound found
  parseLabels = Text.split f . Text.pack where
    f ',' = True
    f ';' = True
    f _   = False
  
  run :: (MonadIO n) => STM a -> n a
  run = liftIO . T.atomicallySync

maybeNotFound :: (MonadError (R.Reason e) m) => Maybe () -> m ()
maybeNotFound = maybe (throwError R.NotFound) return

labelsIndex :: IndexMap.Field (ID.WithID (Decorated o)) Set.Set Text.Text
labelsIndex = IndexMap.namedFields (view labels . ID._object) "labels"

idIndex :: IndexMap.Field (ID.WithID (Decorated o)) Identity (ID.ID (Decorated o))
idIndex = IndexMap.field ID.__ID
