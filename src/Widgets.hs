module Widgets(app) where

import           Actions
import           Brick.AttrMap
import           Brick.Focus                (focusRingCursor)
import           Brick.Forms                (focusedFormInputAttr, formFocus)
import qualified Brick.Main                 as M
import           Brick.Types
import           Brick.Util                 (fg, on)
import           Brick.Widgets.Border       (borderWithLabel)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core
import qualified Brick.Widgets.Edit         as E (editAttr, editFocusedAttr)
import           Control.Monad.IO.Class     (liftIO)
import           Data.List                  as L (elem, intercalate)
import           Data.Text                  as T hiding (center, null)
import           Dialog
import           Form
import qualified Graphics.Vty               as V
import           Lens.Micro                 (each, ix, (%~), (&), (.~), (^.),
                                             (^?))
import           Note
import           Types


helpText = [  "F12            : Show/Hide Help",
              "F1             : Unselect All",
              "Ctrl+Left/Right: Select Note",
              "Ctrl+Up/Down   : Scroll",
              "Ctrl+Del       : Delete",
              "Enter          : Edit",
              "Insert         : Open Dialog",
              "Ctrl-s         : Save & close dialog",
              "Ctrl-c         : Cancel Dialog",
              "Ctrl-c         : Clone selected",
              "Esc            : Save State & Exit"
          ]

drawLayer :: AppState e Name -> Widget Name
drawLayer st = widget
  where widget  | st^.showDialog = noteDialog st
                | not (null (st^.(notes . noteData))) = viewport "MainViewPort" Vertical $ scrollableNoteWidget st
                | otherwise = welcomeWidget

welcomeWidget :: Widget Name
welcomeWidget = center (txt "Welcome to the Notes App")

helpWidget :: AppState e Name -> Widget Name
helpWidget st = result
  where result  | st^.showHelp =  borderWithLabel (str "Help") $
                                  hLimitPercent 15  $
                                  vLimitPercent 100 $
                                  padBottom Max
                                  (txtWrap $ T.pack $ L.intercalate "\n" helpText)
                | otherwise = emptyWidget

appEvent :: AppState e Name -> BrickEvent Name e -> EventM Name (Next (AppState e Name))
appEvent st ev = case ev of
  (VtyEvent (V.EvKey V.KEsc  []))               -> liftIO (onExit st) >>= M.halt
  (VtyEvent (V.EvKey V.KIns  []))               -> M.continue $ st  & showDialog .~ True
  (VtyEvent (V.EvKey (V.KFun 1)  []))           -> M.continue $ st  & selectedIndex .~ (-1)
                                                                    & (notes . noteData . each . selected) .~ False
  (VtyEvent (V.EvKey V.KEnter  []))             -> handleEnter st ev
  (VtyEvent (V.EvKey (V.KChar '\t')  []))       -> handleTab st ev
  (VtyEvent (V.EvKey V.KDown  [V.MCtrl]))       -> M.vScrollBy (M.viewportScroll "MainViewPort") 1 >> M.continue st
  (VtyEvent (V.EvKey V.KUp  [V.MCtrl]))         -> M.vScrollBy (M.viewportScroll "MainViewPort") (-1) >> M.continue st
  (VtyEvent (V.EvKey V.KRight  [V.MCtrl]))      -> select st 1
  (VtyEvent (V.EvKey V.KLeft  [V.MCtrl]))       -> select st (-1)
  (VtyEvent (V.EvKey V.KDel  [V.MCtrl]))        -> remove st
  (VtyEvent (V.EvKey (V.KFun 12) []))           -> M.continue $ st & showHelp %~ not
  (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))  -> cloneOrCancel st
  (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]))  -> onSave st ev >>= M.continue
  (VtyEvent e)                                  -> handleTypingEvents st ev e
  _                                             -> M.continue st

drawUi :: AppState e Name ->  [Widget Name]
drawUi st = [
      withBorderStyle unicode $
      hLimitPercent 100 $
      vLimitPercent 100 $
      helpWidget st <+>
      borderWithLabel (str "Ullekha") (drawLayer st)
    ]

appCursor st  | st^.dialogMode `L.elem` [NoteCreate,NoteEdit] = focusRingCursor formFocus (st^.form)
              | st^.dialogMode `L.elem` [TodoCreate,TodoEdit] = focusRingCursor (^.notes.focusEdit) st
              | otherwise = M.showFirstCursor st

app :: M.App (AppState e Name) e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const theMap
          , M.appChooseCursor =  appCursor
          }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black),
    (E.editFocusedAttr, V.white `on` V.black),
    ("highlightedNote", V.black `on` V.yellow),
    ("normalNote", V.white `on` V.black),
    ("taskHighlighted", fg V.yellow),
    (focusedFormInputAttr, fg V.yellow),
    ("normalTask", fg V.white)
  ]
