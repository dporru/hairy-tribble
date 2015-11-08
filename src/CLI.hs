module CLI where

import qualified Accounts
import           Common
import qualified PH.DB as DB
import           PH.Types
import           PH.Types.JSON ()

import qualified Data.Set                as Set
import qualified Data.TCache             as T
import qualified Data.TCache.ID          as ID
import           Data.Text (pack)
import           Data.Tree (Tree(Node))
import qualified System.Console.Argument as CP
import qualified System.Console.Command  as CP
import qualified System.Console.Program  as CP
import           System.Random.Shuffle (shuffleM)


main :: IO ()
main = do
  account <- input
  DB.initialise $ Accounts.accountStore account
  CP.interactive $ commands account

-- The tree of possible commands of the program.
commands :: Accounts.Account -> CP.Commands IO
commands account = Node
  (CP.command "DB" "Manage the database." . CP.io . CP.showUsage $ commands account)
  [
    Node (CP.command "list" "" . CP.io . CP.showUsage $ commands account)
      [
        Node (CP.command "questions" "List questions." $ CP.io . (>>= mapM_ print) . DB.run account . DB.withStore $ \ s ->
          (ID.listWithID s :: STM [ID.WithID (Decorated Question)])
        ) []
       ,Node (CP.command "tests" "List tests." $ CP.io . (>>= mapM_ print) . DB.run account . DB.withStore $ \ s ->
          (ID.listWithID s :: STM [ID.WithID (Decorated Test)])
        ) []
      ]
  , Node (CP.command "show" "" . CP.io . CP.showUsage $ commands account)
      [
        Node (CP.command "question" "Show a specific question." $
          CP.withNonOption idArg $ \ (i :: ID.ID (Decorated Question)) -> CP.io
            . (>>= maybe (putStrLn "Question not found.") print)
            . DB.run account . DB.withStore $ \ s -> T.readDBRef s (ID.ref i)
        ) []
      , Node (CP.command "test" "Show a specific test." $
          CP.withNonOption idArg $ \ (i :: ID.ID (Decorated Test)) -> CP.io
            . (>>= maybe (putStrLn "Test not found.") print)
            . DB.run account . DB.withStore $ \ s -> T.readDBRef s (ID.ref i)
        ) []
      ]
  , Node (CP.command "new" "" . CP.io . CP.showUsage $ commands account)
      [
        Node (CP.command "question" "Add a new question." . CP.io
          $ (DB.run account . DB.withStore . flip ID.newRef =<< (input :: IO (Decorated Question))) >> putStrLn "Question added."
          ) []
      , Node (CP.command "test" "Add a new test." . CP.io
          $ (DB.run account . DB.withStore . flip ID.newRef =<< (input :: IO (Decorated Test))) >> putStrLn "Test added."
          ) []
      ]
  ]

idArg :: CP.Type (ID.ID a)
idArg = ID.ID . pack <$> CP.string

class Input x where
  input :: IO x

instance Input Question where
  input = do
    q <- putStrLn "Question:" >> input
    a <- putStrLn "Answer:" >> input
    putStrLn "Enter more answers for multiple choice, empty line when done:"
    others <- map plainText <$> askMany
    if null others
      then return $ Question q (Open a)
      else do
        let answers = (True,a) : map ((,) False) others
        randomOrder <- shuffleM [0 .. length others]
        return $ Question q (MultipleChoice answers randomOrder)

instance Input Test where
  input = do
    name <- prompt "Name:"
    qs <- map (TestQuestion . ID.ref . ID.ID . pack) <$> promptF read "Questions:"
    return $ Test name qs

instance (Input x) => Input (Dated x) where
  input = date =<< input

instance (Input x) => Input (Labelled x) where
  input = do
    x <- input
    putStrLn "Labels:"
    ls <- Set.fromList <$> askMany
    return $ Labelled ls x

instance Input Accounts.Account where
  input = promptF Accounts.fromString "Enter account:"

instance Input RichText where
  input = plainText <$> input

instance Input Text where
  input = pack <$> getLine

prompt :: String -> IO Text
prompt = promptF pack

promptF :: (String -> a) -> String -> IO a
promptF f s = do
  putStrLn s
  f <$> getLine

askMany :: IO [Text]
askMany = do
  x <- getLine
  if null x
    then return []
    else (pack x :) <$> askMany
