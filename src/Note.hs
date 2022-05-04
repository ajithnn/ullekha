module Note where

import           Brick.Focus
import           Brick.Forms
import           Brick.Types
import           Brick.Widgets.Center (center, centerAbout)
import           Brick.Widgets.Core
import           Brick.Widgets.Dialog as D
import           Brick.Widgets.Edit
import           Data.List            as L (length)
import           Data.Text            as T hiding (map, unlines)
import           Form
import           Lens.Micro           (ix, non, (%~), (&), (.~), (^.), (^?))
import           Types


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

getTodoTitle :: AppState e Name -> Text
getTodoTitle st = T.pack $ unlines $ getEditContents (st^.notes.taskTitle)

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
