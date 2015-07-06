module PH.Types.Dated
  (
    Dated(Dated)
  , Dates(..)
  , date
  , deleted
  , deleteDated
  , updateDated
  , getCurrentTime
  ) where

import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Data.Time              (UTCTime,getCurrentTime)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)


data Dated x
  = Dated Dates x
  deriving (Generic,Typeable,Show)

data Dates
  = Dates
    {
      creationDate     :: UTCTime
    , modificationDate :: UTCTime
    , deletionDate     :: Maybe UTCTime
    }
  deriving (Generic,Typeable,Show)

date :: (MonadIO m) => x -> m (Dated x)
date x = do
  t <- liftIO getCurrentTime
  let d = Dates t t Nothing
  return $ Dated d x

deleted :: Dated x -> Bool
deleted (Dated d _) = case deletionDate d of
  Nothing -> False 
  Just _  -> True

deleteDated :: UTCTime -> Dated x -> Dated x
deleteDated t (Dated d x) = Dated d' x where
  d' = d { deletionDate = case deletionDate d of
    Nothing -> Just t
    Just t' -> Just t'
    }

updateDated :: UTCTime -> Dated x -> Dated x
updateDated t (Dated d x) = Dated d' x where
  d' = d { modificationDate = t }
