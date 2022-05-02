module Dialog where


import           Brick.Focus
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center       as C (center, centerAbout, hCenter)
import           Brick.Widgets.Core
import           Brick.Widgets.Dialog       as D
import           Brick.Widgets.Edit
import           Data.Text                  hiding (elem, map, unlines)
import           Form
import           Lens.Micro                 (ix, non, (%~), (&), (.~), (^.),
                                             (^?))
import           Note
import           Task
import           Types

getDialog :: D.Dialog Choice
getDialog = D.dialog (Just "Note") (Just (0,choices)) 45
  where choices = [("(CTRL-S) Save",Save),("(CtRL-C) Cancel",Cancel)]

noteDialog :: AppState e Name -> Widget Name
noteDialog st = padLeft (Pad 0) $ padRight Max $ padBottom Max dlgContent
    where nDialog = st^.dlg
          dlgContent  | st^.dialogMode  == ChoiceCreate = renderDialog nDialog $ choiceDialog st
                      | st^.dialogMode  `elem` [TodoCreate,TodoEdit] = renderDialog nDialog $ vBox $ [todoTitleWidget st] ++ [str "\n"] ++ getTasks st ++ [newTaskWidget st]
                      | st^.dialogMode  `elem` [NoteCreate,NoteEdit] = renderDialog nDialog $ getForm $ st^.form
                      | otherwise = renderDialog nDialog $ str "Invalid Mode"

choiceDialog st =  C.hCenter (nHighlighAttr (withBorderStyle unicode (border $ str "Note")) <+> tHighlighAttr (withBorderStyle unicode (border $ str  "Todo")))
  where nHighlighAttr | st^.dialogSelect == NoteCreate = withAttr "highlightedNote"
                      | otherwise = withAttr "normalNote"
        tHighlighAttr | st^.dialogSelect == TodoCreate = withAttr "highlightedNote"
                      | otherwise = withAttr "normalNote"


