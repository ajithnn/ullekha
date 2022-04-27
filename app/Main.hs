module Main where

import           Actions
import qualified Brick.Main         as M (defaultMain)
import           Control.Monad      (void)
import           Data.List          as L (length)
import           Form
import           System.Environment
import           Types
import           Widgets

main :: IO ()
main =  void $ getArgs >>= onStart >>= M.defaultMain app
