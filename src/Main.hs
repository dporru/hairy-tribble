module Main
  (
    main
  ) where


import           Common
import           PH.API (M,api)
import qualified PH.DB                   as DB
import           Server (serve)

import qualified System.Console.Argument as CP
import qualified System.Console.Command  as CP
import qualified System.Console.Program  as CP


main :: IO ()
main = do
  DB.initialise
  CP.single commands

commands :: CP.Commands IO
commands = flip CP.Node [] . CP.command "serve" "" $
  CP.withOption serverHostOption   $ \ serverHost   ->
  CP.withOption googleIDOption     $ \ googleID     ->
  CP.withOption googleSecretOption $ \ googleSecret ->
    CP.io $ serve googleID googleSecret serverHost

serverHostOption :: CP.Option String
serverHostOption = CP.option [] ["serverHost"] CP.string "http://localhost:8000/" ""

googleIDOption :: CP.Option String
googleIDOption = CP.option [] ["googleID"] CP.string (error "google OAuth2 ID not specified") ""

googleSecretOption :: CP.Option String
googleSecretOption = CP.option [] ["googleSecret"] CP.string (error "google OAuth2 secret not specified") ""
