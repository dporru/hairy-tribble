module PH.API
  (
    api
  ) where

import qualified PH.API.Question    as Question
import qualified PH.API.Test        as Test
import qualified PH.API.Test.Export as Export

import           Rest.Api (Api,mkVersion,Some1(Some1),Router,root,route,(-/),(--/))

api :: Api IO
api = [(mkVersion 0 0 0,Some1 ph)]

ph :: Router IO IO
ph = root -/ route Question.resource
          -/ route Test.resource --/ route Export.resource
