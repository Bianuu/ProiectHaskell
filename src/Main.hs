module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
   DB.save DB.empty
   putStrLn "init"
--handleInit = return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  dbLoad <- DB.load
  case dbLoad of
    Error err  -> putStrLn "Failed to load DB"
    Success dab -> let
                  gaseste = DB.findFirst (\en -> entryId en == getOptId getOpts) <$> dbLoad
                  in
                    case gaseste of
                      Success rasp -> case rasp of
                                        Just anAnswer -> putStrLn $ entrySnippet anAnswer
                                        _ -> putStrLn "Faild to find"
                      _ -> putStrLn "Faild to find"
--handleGet getOpts = return ()

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  loadDataBase <- DB.load
  case loadDataBase of 
    (Error e) -> putStrLn "Failed to load DB"
    (Success succ) -> do
      let find = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) succ
      case find of
        [] -> putStrLn "No entries found"
        _ -> putStrLn $ unlines $ map (head . lines . show . FmtEntry) find


--handleSearch searchOpts = return ()

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  db <- DB.load
  rf <- readFile (addOptFilename addOpts)
  case db of
    Success success -> do
      let findFirstFunction = DB.findFirst (\f -> rf == entrySnippet f) success
      case findFirstFunction of
        Just i -> do
          putStrLn "Entry with this content already exists: "
          print (Entry.Entry.FmtEntry i)
          return ()
        Nothing -> do
          let add = getSuccess db DB.empty
              insertad = DB.insertWith (\id -> makeEntry id rf addOpts) add
          DB.save insertad
          return ()
    Error error -> do
      putStrLn "Failed to load DB"
      return ()
  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
