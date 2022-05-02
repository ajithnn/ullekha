module Types where


import           Brick.Focus
import           Brick.Forms
import           Brick.Widgets.Dialog
import           Brick.Widgets.Edit
import           Data.Aeson
import           Data.Text            as T hiding (center)
import           GHC.Generics
import           Lens.Micro.TH        (makeLenses)
import           System.FilePath

type Name = Text

data CmdInputOptions = FileInput FilePath | NoPersist

data Choice = Save | Cancel deriving (Show)


data NoteMode = TodoList | FreeNote | InvalidMode deriving (Show,Eq,Generic,Ord)

data DialogMode = TodoCreate | NoteCreate | TodoEdit | NoteEdit | ChoiceCreate deriving (Show,Eq,Generic,Ord)

instance ToJSON NoteMode where
instance FromJSON NoteMode where

data Task = Task{
  _status       :: Bool,
  _selectedTask :: Bool,
  _task         :: Text
} deriving (Generic,Eq)

instance Show Task where
  show (Task s _ t) = mconcat [tick s,T.unpack t]
    where tick s  | s = "[X] "
                  | otherwise = "[ ] "

instance ToJSON Task where
instance FromJSON Task where

data Note = Note{
  _title             :: Text,
  _content           :: Text,
  _selected          :: Bool,
  _tasks             :: [Task],
  _selectedTaskIndex :: Int,
  _mode              :: NoteMode,
  _highlighted       :: Bool
} deriving (Show,Generic,Eq)

instance ToJSON Note where
instance FromJSON Note where

data Notes = Notes{
  _totalNotes   :: Int,
  _noteData     :: [Note],
  _tempTodoNote :: Note,
  _taskEdit     :: Editor String Name,
  _taskTitle    :: Editor String Name,
  _focusEdit    :: FocusRing Name
}

data AppState e n = AppState {
  _notes         :: Notes,
  _selectedIndex :: Int,
  _showDialog    :: Bool,
  _dialogMode    :: DialogMode,
  _dialogSelect  :: DialogMode,
  _showHelp      :: Bool,
  _form          :: Form Note e n,
  _editMode      :: Bool,
  _persistFile   :: FilePath,
  _dlg           :: Dialog Choice

}

makeLenses ''Notes
makeLenses ''Note
makeLenses ''Task
makeLenses ''AppState
