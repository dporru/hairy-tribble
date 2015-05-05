module PH.API.Test.Export.Markdown
  (
    export
  ) where

import           Common

import           Data.ByteString.Lazy         (ByteString)
import qualified Text.Pandoc                as P
import qualified Text.Pandoc.Options        as P


export:: P.Pandoc -> IO (Either ByteString ByteString)
export = return . Right . utf8ByteString . P.writeMarkdown P.def
