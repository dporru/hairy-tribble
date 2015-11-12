module Main
  (
    main
  ) where

import           Accounts  (Accounts,accountsOptionType,accountStore)
import           Common
import           PH.API    (M,api)
import qualified PH.DB                   as DB
import           Server    (serve)

import           Data.List (nub)
import qualified Data.Map                as Map
import qualified System.Console.Argument as CP
import qualified System.Console.Command  as CP
import qualified System.Console.Program  as CP


main :: IO ()
main = CP.single commands

commands :: CP.Commands IO
commands = flip CP.Node [] . CP.command "serve" "" $
  CP.withOption serverHostOption   $ \ serverHost   ->
  CP.withOption googleIDOption     $ \ googleID     ->
  CP.withOption googleSecretOption $ \ googleSecret ->
  CP.withOption accountsOption     $ \ accounts     -> CP.io $ do
    for_ (nub . Map.elems $ accounts) $
      DB.initialise . accountStore
    serve googleID googleSecret serverHost accounts

serverHostOption :: CP.Option String
serverHostOption = CP.option [] ["serverHost"] CP.string
  "http://localhost:8000/" ""

googleIDOption :: CP.Option String
googleIDOption = CP.option [] ["googleID"] CP.string
  (error "google OAuth2 ID not specified") ""

googleSecretOption :: CP.Option String
googleSecretOption = CP.option [] ["googleSecret"] CP.string
  (error "google OAuth2 secret not specified") ""

accountsOption :: CP.Option Accounts
accountsOption = CP.option [] ["accounts"] accountsOptionType
  (error "no accounts specified") ""
