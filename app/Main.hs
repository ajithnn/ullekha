module Main where

import           Actions
import qualified Brick.Main          as M (defaultMain)
import           Control.Monad       (void)
import           Data.Semigroup      ((<>))
import           Form
import           Options.Applicative
import           Types
import           Widgets

main :: IO ()
main =  void $ execParser opts >>= onStart >>= M.defaultMain app

input :: Parser CmdInputOptions
input = FileInput
        <$> strOption
          (  long "file"
            <> short 'f'
            <> metavar "FILENAME"
            <> help "File to use for persistece of Note data for both input/output" )
        <|> RedisInput
          <$>
          strOption
           ( long "redis"
            <> short 'r'
            <> metavar "redis://username:password@host:6379/0"
            <> help "Redis connection string for persistence, this can be remote redis" )
        <|> flag' NoPersist
          (  long "no-persist"
            <> help "In-memory notes storage, deletes upon exit" )

fullOptions :: Parser CmdOptions
fullOptions = CmdOptions <$>
                input
                  <*>
                    strOption
                      ( long "redis-key"
                      <> short 'k'
                      <> metavar "REDIS-KEY-TO-STORE-NOTES"
                      <> showDefault
                      <> value "ullekha_notes"
                      <> help "Key to use on the redis server to store the notes" )


opts = info (fullOptions <**> helper)
        ( fullDesc
          <> progDesc "Notes App with / without Persistence"
          <> header "ullekha - Notes on the terminal" )

