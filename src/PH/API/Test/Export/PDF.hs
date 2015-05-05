module PH.API.Test.Export.PDF
  (
    export
  ) where

import           Common
import qualified PH.API.Test.Export.LaTeX as LaTeX

import           Data.ByteString.Lazy (ByteString)
import qualified Text.Pandoc          as P
import qualified Text.Pandoc.Builder  as P
import qualified Text.Pandoc.Options  as P
import qualified Text.Pandoc.PDF      as P


export:: P.Pandoc -> IO (Either ByteString ByteString)
export = P.makePDF "xelatex" P.writeLaTeX
  P.def
    {
      P.writerStandalone = True
    , P.writerTemplate   = LaTeX.template
    , P.writerVariables  = [("mainfont","DejaVu Sans")]
    } 
