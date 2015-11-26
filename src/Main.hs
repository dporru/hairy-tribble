module Main
  (
    main
  ) where

import           Accounts  (Accounts,accountsOptionType)
import           Common
import           PH.API    (M,api)
import qualified PH.DB                   as DB
import           Server    (serve)

import           Control.Concurrent (myThreadId)
import           Control.Exception
  ( throwTo
  , AsyncException(UserInterrupt)
  , finally
  )
import           Data.List (nub)
import qualified Data.Map                as Map
import qualified System.Console.Argument as CP
import qualified System.Console.Command  as CP
import qualified System.Console.Program  as CP
import           System.Posix.Signals
  ( installHandler
  , sigTERM
  , Handler(CatchOnce)
  )


main :: IO ()
main = do
  threadId <- myThreadId
  installHandler sigTERM
    (CatchOnce $ do
      putStrLn "Program caught sigTERM, interrupting..."
      throwTo threadId UserInterrupt
    ) Nothing
  CP.single commands

commands :: CP.Commands IO
commands = flip CP.Node [] . CP.command "serve" "" $
  CP.withOption serverHostOption   $ \ serverHost   ->
  CP.withOption googleIDOption     $ \ googleID     ->
  CP.withOption googleSecretOption $ \ googleSecret ->
  CP.withOption accountsOption     $ \ accounts     -> CP.io $ do
    (stores,finals) <- fmap unzip . for (nub . Map.elems $ accounts) $ \ a -> do
      store <- DB.initialise a
      return ((a, store), DB.finalise store)
    serve googleID googleSecret serverHost accounts (Map.fromList stores)
      `finally` (sequence_ finals)

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
