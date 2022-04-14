module Widgets(app) where

import           Actions
import           Brick
import           Brick.AttrMap
import           Brick.Focus                (focusGetCurrent, focusRing,
                                             focusRingCursor)
import           Brick.Forms
import qualified Brick.Main                 as M
import           Brick.Types
import           Brick.Widgets.Border       (border, borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core
import           Brick.Widgets.Dialog       as D
import qualified Brick.Widgets.Edit         as E
import           Data.List                  as L (map)
import           Dialog
import           Form
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((%~), (&), (.~), (^.))
import           Types                      as Ty


drawLayer :: AppState e Name -> Widget Name
drawLayer st = widget
  where widget  | st^.showDialog = noteDialog st
                | st^.(notes .totalNotes) > 0 = noteWidgets st
                | otherwise = welcomeWidget

welcomeWidget :: Widget Name
welcomeWidget = center (txt "Welcome to the Notes App")

noteWidgets :: AppState e Name -> Widget Name
noteWidgets st = padLeft (Pad 0) $ padRight Max $ padBottom Max $
  withBorderStyle unicode $
  hBox $
  L.map  note (st^.(notes . noteData))
    where
      note :: Note -> Widget Name
      note n =  hLimit 35 $
                      vLimit 20 $
                      borderWithLabel (txt $ n^.title)
                      (center $ txt $ n^.content)

appEvent :: AppState e Name -> BrickEvent Name e -> EventM Name (Next (AppState e Name))
appEvent st ev = case ev of
  (VtyEvent (V.EvKey V.KEsc  []))               -> M.halt st
  (VtyEvent (V.EvKey V.KIns  []))               -> M.continue $ st & showDialog .~ True
  (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))  -> M.continue $ st & showDialog .~ False
  (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]))  -> onSave st ev
  _                                             -> onFormUpdate st ev

drawUi :: AppState e Name ->  [Widget Name]
drawUi st = [
      withBorderStyle unicode $
      hLimitPercent 100 $
      vLimitPercent 100 $
      borderWithLabel (str "Notes") $
      drawLayer st
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
