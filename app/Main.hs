module Main where

import           Actions
import qualified Brick.Main    as M (defaultMain)
import           Control.Monad (void)
import           Data.List     as L (length)
import           Dialog
import           Form
import           Types
import           Widgets

main :: IO ()
main =  void $ initAppState >>= M.defaultMain app

initAppState :: IO (AppState e Name)
initAppState = onStart >>= (\notes ->
                return AppState{
                _notes = Notes {
                    _totalNotes = L.length notes,
                    _currentNote = 0,
                    _noteData = notes
                  },
                _form = emptyForm,
                _showDialog = False,
                _showHelp = True,
                _dlg = getDialog
              })
