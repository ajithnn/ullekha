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
input = FileInput <$> strOption
          (  long "file"
            <> short 'f'
            <> metavar "FILENAME"
            <> help "File to use for persistece of Note data for both input/output" )
        <|> flag' NoPersist
          (  long "no-persist"
            <> help "In-memory notes storage, deletes upon exit" )


opts = info (input <**> helper)
        ( fullDesc
          <> progDesc "Notes App with / without Persistence"
          <> header "ullekha - Notes on the terminal" )

