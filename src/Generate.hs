module Generate where

import           PH.API (api)

import qualified Rest.Gen        as Gen
import qualified Rest.Gen.Config as Gen

main :: IO ()
main = do
  config <- Gen.configFromArgs "generate -d localhost:8000/ --source=rest-gen-files/Docs/"
  Gen.generate config "PH" api [] [] []
