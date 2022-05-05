module Note where

import           Brick.Focus
import           Brick.Forms
import           Brick.Types
import           Brick.Widgets.Border       (border, borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (borderStyleFromChar, unicode,
                                             unicodeBold, unicodeRounded)
import           Brick.Widgets.Center       (center, centerAbout)
import           Brick.Widgets.Core
import           Brick.Widgets.Dialog       as D
import           Brick.Widgets.Edit
import           Data.List                  as L (length, map, transpose)
import           Data.List.Split            (chunksOf)
import           Data.Text                  as T hiding (chunksOf, map, unlines)
import           Form
import           Lens.Micro                 (ix, non, (%~), (&), (.~), (^.),
                                             (^?))
import           Types

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
        noteOrTodo  | n^.mode == FreeNote = borderWithLabel (txt $ n^.title) (padTop (Pad 0) $ highlightStyle $ txtWrap $ n^.content)
                    | n^.mode == TodoList = borderWithLabel (txt $ n^.title) (padTop (Pad 0) $ highlightStyle $ (strWrap . unlines) (L.map show (n^.tasks)))
                    | otherwise = emptyWidget

initNotes :: [Note] -> Notes
initNotes notes = Notes{
                    _taskEdit = editor "TaskEdit" Nothing "",
                    _taskEditLabel = "New Task",
                    _taskTitle = editor "TaskTitle" Nothing "",
                    _focusEdit = focusRing ["TaskTitle","TaskEdit"],
                    _totalNotes = L.length notes,
                    _taskEditMode = False,
                    _noteData = notes,
                    _tempTodoNote = getTodoNote "" []
                    }

getFreeNote :: Form Note e Name -> AppState e Name -> Bool -> Note
getFreeNote f' st sel = Note{
                  _title = formState f'^.title,
                  _content = formState f'^.content,
                  _selected = sel,
                  _highlighted=formState f'^.highlighted,
                  _tasks = [],
                  _selectedTaskIndex = -1,
                  _mode = FreeNote
                }


getTodoNote :: Text -> [Task] -> Note
getTodoNote title tsk = Note{
                  _title = title,
                  _content = "",
                  _selected = False,
                  _highlighted=False,
                  _tasks = tsk,
                  _selectedTaskIndex = -1,
                  _mode = TodoList
                }

createTodoNote st = updatedTempNote ^. (notes . tempTodoNote)
  where updatedTempNote = st & (notes . tempTodoNote . title) .~ getTodoTitle st
        getTodoTitle st = T.pack $ unlines $ getEditContents (st^.notes.taskTitle)

emptyNote = Note{
                  _title = "",
                  _selectedTaskIndex = -1,
                  _content = "",
                  _selected = False,
                  _highlighted=False,
                  _mode = InvalidMode,
                  _tasks = []
            }

getMode st = (st^.(notes . noteData) ^? ix (st^.selectedIndex)) ^. (non emptyNote . mode)

newTaskWidget st = txt (mconcat [st^.notes.taskEditLabel,": "]) <+> hLimit 30 (vLimit 1 $ withFocusRing (st^.(notes .focusEdit)) (renderEditor (str . unlines)) (st^.(notes . taskEdit)))

todoTitleWidget st = str "Title:    " <+> hLimit 30 (vLimit 1 $ withFocusRing (st^.(notes .focusEdit)) (renderEditor (str . unlines)) (st^.(notes . taskTitle)))

emptyEditor :: Name -> Editor String Name
emptyEditor name = editor name Nothing ""
