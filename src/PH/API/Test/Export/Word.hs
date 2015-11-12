module PH.API.Test.Export.Word
  (
    export
  ) where

import           Common

import           Data.ByteString.Lazy         (ByteString)
import qualified Text.Pandoc                as P
import qualified Text.Pandoc.Options        as P


export:: (MonadIO m) => P.Pandoc -> m (Either ByteString ByteString)
export = fmap Right . liftIO . P.writeDocx P.def
