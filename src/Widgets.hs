module Widgets(app) where

import           Actions
import           Brick                      (on)
import           Brick.AttrMap
import           Brick.Focus                (focusGetCurrent, focusRing,
                                             focusRingCursor)
import           Brick.Forms
import qualified Brick.Main                 as M
import           Brick.Types
import           Brick.Widgets.Border       (border, borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center, centerLayer, hCenter)
import           Brick.Widgets.Core
import           Brick.Widgets.Dialog       as D
import qualified Brick.Widgets.Edit         as E
import           Control.Monad.IO.Class     (liftIO)
import           Data.List                  as L (map)
import           Data.List.Split
import           Dialog
import           Form
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((%~), (&), (.~), (^.))
import           Types                      as Ty


drawLayer :: AppState e Name -> Widget Name
drawLayer st = widget
  where widget  | st^.showDialog = noteDialog st
                | st^.(notes .totalNotes) > 0 = viewport MainViewPort Vertical $ customWidget st
                | otherwise = welcomeWidget

welcomeWidget :: Widget Name
welcomeWidget = center (txt "Welcome to the Notes App")

customWidget :: AppState e Name -> Widget Name
customWidget s =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let totalWidth = ctx^.availWidthL
        render $ vLimitPercent 100 $ noteWidgets totalWidth s

noteWidgets :: Int -> AppState e Name -> Widget Name
noteWidgets width st = padLeft (Pad 0) $ padRight Max $ padBottom Max $
  withBorderStyle unicode widgetLayout
    where rows = splitNotes width 35 (st^.(notes . noteData))
          widgetLayout =  vBox $ L.map (hBox . L.map note) rows
          splitNotes totWidth noteWidth = chunksOf $ totWidth `div` noteWidth

note :: Note -> Widget Name
note n =  hLimit 35 $
          vLimit 20 $
          borderWithLabel (txt $ n^.title)
          (padTop (Pad 0) $ txtWrap $ n^.content)

helpWidget :: AppState e Name -> Widget Name
helpWidget st = result
  where result  | st^.showHelp =  borderWithLabel (str "Help")
                                  (txtWrap "F12 - Show/Hide Help; Insert - Open Dialog; Ctrl-s : Save Note; Ctrl-c : Cancel Dialog; Ctrl-Up/Down : Scroll Viewport; Esc : Save State & Exit")
                | otherwise = emptyWidget

appEvent :: AppState e Name -> BrickEvent Name e -> EventM Name (Next (AppState e Name))
appEvent st ev = case ev of
  (VtyEvent (V.EvKey V.KEsc  []))               -> liftIO (onExit st) >>= M.halt
  (VtyEvent (V.EvKey V.KIns  []))               -> M.continue $ st & showDialog .~ True
  (VtyEvent (V.EvKey V.KDown  [V.MCtrl]))       -> scroll st 1
  (VtyEvent (V.EvKey V.KUp  [V.MCtrl]))         -> scroll st (-1)
  (VtyEvent (V.EvKey (V.KFun 12) []))           -> M.continue $ st & showHelp %~ not
  (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))  -> M.continue $ st & showDialog .~ False
  (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]))  -> onSave st ev
  _                                             -> onFormUpdate st ev

drawUi :: AppState e Name ->  [Widget Name]
drawUi st = [
      withBorderStyle unicode $
      hLimitPercent 100 $
      vLimitPercent 100 $
      helpWidget st <=>
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
  [ (E.editAttr, V.white `on` V.black)
  ]
