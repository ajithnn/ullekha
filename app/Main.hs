module Main where

import           Brick                      (Widget, emptyWidget, simpleMain,
                                             str, withBorderStyle, (<+>))
import           Brick.AttrMap              (attrMap)
import qualified Brick.Main                 as M
import           Brick.Types
import           Brick.Widgets.Border       (borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center, hCenter)
import           Brick.Widgets.Core
import           Control.Monad              (void)
import           Data.List                  as L (map)
import           Data.Text                  as T hiding (center)
import           Dialog
import           Form
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((%~), (&), (^.))
import           Lens.Micro.TH              (makeLenses)
import           Types
import           Widgets

main :: IO ()
main =  void $
        M.defaultMain app initAppState

initAppState :: AppState e Name
initAppState = AppState{
                _notes = Notes {
                    _totalNotes = 0,
                    _currentNote = 0,
                    _noteData = []
                  },
                _form = emptyForm,
                _showDialog = False,
                _dlg = getDialog
              }
