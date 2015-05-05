module DB where

import           Common
import qualified PH.DB as DB
import           PH.Types
import           PH.Types.JSON ()

import qualified Data.TCache             as T
import qualified Data.TCache.ID          as ID
import qualified Data.TCache.IndexQuery  as T
import           Data.Text (pack)
import           Data.Tree (Tree(Node))
import qualified System.Console.Argument as CP
import qualified System.Console.Command  as CP
import qualified System.Console.Program  as CP
import           System.Random.Shuffle (shuffleM)


main :: IO ()
main = do
  DB.initialiseIndices
  CP.interactive commands

commands :: CP.Commands IO
commands = Node
  (CP.Command "DB" "Manage the database." $ CP.io $ CP.showUsage commands)
  [
    Node (CP.Command "list" "" $ CP.io $ CP.showUsage commands)
      [
        Node (CP.Command "questions" "List questions." $ CP.io . (>>= mapM_ print) . DB.run
          $ (ID.listWithID :: STM [ID.WithID Question])
        ) []
       ,Node (CP.Command "tests" "List tests." $ CP.io . (>>= mapM_ print) . DB.run
          $ (ID.listWithID :: STM [ID.WithID Test])
        ) []
      ]
  , Node (CP.Command "show" "" $ CP.io $ CP.showUsage commands)
      [
        Node (CP.Command "question" "Show a specific question." $
          CP.withNonOption idArg $ \ i -> CP.io
            . (>>= maybe (putStrLn "Question not found.") (print :: Question -> IO ()))
            . DB.run $ ID.maybeDeref (ID.lookup i)
        ) []
      , Node (CP.Command "test" "Show a specific test." $
          CP.withNonOption idArg $ \ i -> CP.io
            . (>>= maybe (putStrLn "Test not found.") (print :: Test -> IO ()))
            . DB.run $ ID.maybeDeref (ID.lookup i)
        ) []
      ]
  , Node (CP.Command "new" "" $ CP.io $ CP.showUsage commands)
      [
        Node (CP.Command "question" "Add a new question." $ CP.io
          $ (DB.run . ID.newRef =<< (input :: IO Question)) >> putStrLn "Question added."
          ) []
      , Node (CP.Command "test" "Add a new test." $ CP.io
          $ (DB.run . ID.newRef =<< (input :: IO Test)) >> putStrLn "Test added."
          ) []
      ]
  ]

idArg :: CP.Type ID.ID
idArg = fromInteger <$> CP.integer

class Input x where
  input :: IO x

instance Input Question where
  input = do
    q <- ask "Question:"
    a <- ask "Answer:"
    putStrLn "Enter more answers for multiple choice, empty line when done:"
    others <- askMany
    if null others
      then return . Question q $ Open a
      else do
        randomOrder <- shuffleM [0 .. length others]
        return $ Question q $ MultipleChoice a others randomOrder

instance Input Test where
  input = do
    name <- ask "Name:"
    qs <- map ID.lookup <$> askF read "Questions:"
    return $ Test name qs

ask :: String -> IO Text
ask = askF pack

askF :: (String -> a) -> String -> IO a
askF f s = do
  putStrLn s
  f <$> getLine

askMany :: IO [Text]
askMany = do
  x <- getLine
  if null x
    then return []
    else (pack x :) <$> askMany
