module Actions (onSave,onFormUpdate,scroll,onExit,onStart) where

import           Brick
import           Brick.Forms
import qualified Brick.Main            as M
import           Brick.Types
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy  as BL hiding (readFile)
import           Form
import           Lens.Micro            ((%~), (&), (.~), (^.))
import           Types                 as Ty


onFormUpdate st ev = do
  if st^.showDialog then do
    f' <- handleFormEvent ev $ st^.form
    M.continue $ st & form .~ f'
  else do M.continue st

onSave st ev = do
  mExtent <- M.lookupExtent NotesRow
  f' <- handleFormEvent ev $ st^.form
  M.continue $ st & (notes . noteData) %~ (++[Note{_title = formState f'^.title,_content = formState f'^.content}])
                  & showDialog .~ False
                  & form .~ emptyForm
                  & (notes . totalNotes) %~ (+1)

scroll st by = do
  let vp = viewportScroll MainViewPort
  vScrollBy vp by
  M.continue st

onExit st = do
  encodeFile "/home/ajithn/haskell/modules/ullekha/file.txt" $ st^.notes.noteData
  return st

onStart = do
  contents <- readFile "/home/ajithn/haskell/modules/ullekha/file.txt"
  let response = eitherDecode ((BL.fromStrict . B.pack) contents) :: Either String [Note]
  case response of
    Left e      -> return []
    Right notes -> return notes
