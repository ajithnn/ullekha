module Types where


import           Brick.Forms
import           Brick.Widgets.Dialog
import           Data.Aeson
import           Data.Text            as T hiding (center)
import           GHC.Generics
import           Lens.Micro.TH        (makeLenses)


data Name = TitleField
          | ContentField
          | NotesRow
          | MainViewPort
          | Halted
          deriving (Eq, Ord, Show)

data Choice = Save | Cancel deriving (Show)

data Note = Note{
  _title   :: Text,
  _content :: Text
} deriving (Show,Generic)

instance ToJSON Note where
instance FromJSON Note where

--data Status = New | Done | Important | Removed deriving (Show)

data Notes = Notes{
  _totalNotes  :: Int,
  _currentNote :: Int,
  _noteData    :: [Note]
} deriving (Show)


data AppState e n = AppState {
  _notes      :: Notes,
  _showDialog :: Bool,
  _showHelp   :: Bool,
  _form       :: Form Note e n,
  _dlg        :: Dialog Choice

}


makeLenses ''Notes
makeLenses ''Note
makeLenses ''AppState





