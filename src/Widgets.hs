module Widgets(app) where

import           Actions
import           Brick                      (on)
import           Brick.AttrMap
import           Brick.Focus                (focusGetCurrent, focusNext,
                                             focusRing, focusRingCursor)
import           Brick.Forms
import qualified Brick.Main                 as M
import           Brick.Types
import           Brick.Util                 (fg, on)
import           Brick.Widgets.Border       (border, borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (borderStyleFromChar, unicode,
                                             unicodeBold, unicodeRounded)
import           Brick.Widgets.Center       (center, centerLayer, hCenter)
import           Brick.Widgets.Core
import           Brick.Widgets.Dialog       as D
import qualified Brick.Widgets.Edit         as E
import           Control.Monad.IO.Class     (liftIO)
import           Data.List                  as L (intercalate, length, map,
                                                  transpose)
import           Data.List.Split
import           Data.Text                  as T hiding (center, chunksOf, null,
                                                  unlines)
import           Dialog
import           Form
import qualified Graphics.Vty               as V
import           Lens.Micro                 (each, ix, (%~), (&), (.~), (^.),
                                             (^?))
import           Types                      as Ty


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

scrollableNoteWidget :: AppState e Name -> Widget Name
scrollableNoteWidget s =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let totalWidth = ctx^.availWidthL
        render $ vLimitPercent 100 $ noteWidgets totalWidth s

noteWidgets :: Int -> AppState e Name -> Widget Name
noteWidgets width st = padLeft (Pad 0) $ padRight Max $ padBottom Max $
  withBorderStyle unicode widgetLayout
    where rows = L.transpose $ splitNotes width 35 (st^.(notes . noteData))
          widgetLayout =  hBox $ L.map (vBox . L.map note) rows
          splitNotes totWidth noteWidth = chunksOf $ totWidth `div` noteWidth

note :: Note -> Widget Name
note n =  withBorderStyle borderStyle $
          hLimit 35 $
          vLimit 20 noteOrTodo
  where borderStyle     | n^.selected = unicodeBold
                        | otherwise = unicode
        highlightStyle  | n^.highlighted = withAttr "highlightedNote"
                        | otherwise = withAttr "normalNote"
        noteOrTodo  | n^.mode == FreeNote = borderWithLabel (txt $ n^.title)
                                              (padTop (Pad 0) $ highlightStyle $ txtWrap $ n^.content)
                    | n^.mode == TodoList = borderWithLabel (txt $ n^.title)
                                              (padTop (Pad 0) $ highlightStyle $ (strWrap . unlines) (L.map show (n^.tasks)))
                    | otherwise = emptyWidget

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
  (VtyEvent (V.EvKey V.KDown  [V.MCtrl]))       -> scroll st 1
  (VtyEvent (V.EvKey V.KUp  [V.MCtrl]))         -> scroll st (-1)
  (VtyEvent (V.EvKey V.KRight  [V.MCtrl]))      -> select st 1
  (VtyEvent (V.EvKey V.KLeft  [V.MCtrl]))       -> select st (-1)
  (VtyEvent (V.EvKey V.KDel  [V.MCtrl]))        -> remove st
  (VtyEvent (V.EvKey (V.KFun 12) []))           -> M.continue $ st & showHelp %~ not
  (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))  -> cloneOrCancel st
  (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]))  -> onSave st ev
  (VtyEvent e)                                  -> handleTypingEvents st ev e
  _                                             -> M.continue st

drawUi :: AppState e Name ->  [Widget Name]
drawUi st = [
      withBorderStyle unicode $
      hLimitPercent 100 $
      vLimitPercent 100 $
      helpWidget st <+>
      borderWithLabel (str "Notes") (drawLayer st)
    ]

appCursor st = focusRingCursor formFocus $ st^.form

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
    (E.editFocusedAttr, V.black `on` V.yellow),
    ("highlightedNote", V.black `on` V.yellow),
    ("normalNote", V.white `on` V.black),
    ("taskHighlighted", fg V.yellow),
    ("normalTask", fg V.white)
  ]
