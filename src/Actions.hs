module Actions (
  onSave,
  onFormUpdate,
  scroll,
  onExit,
  onStart,
  remove,
  edit,
  cloneOrCancel,
  select
  ) where

import           Brick
import           Brick.Forms
import qualified Brick.Main            as M
import           Brick.Types
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy  as BL hiding (readFile)
import           Data.List             as L (delete, head, length)
import           Data.Maybe
import           Dialog
import           Form
import           Lens.Micro            (each, ix, (%~), (&), (.~), (^.), (^?))
import           Types                 as Ty


onFormUpdate st ev = do
  if st^.showDialog then do
    f' <- handleFormEvent ev $ st^.form
    M.continue $ st & form .~ f'
  else do M.continue st

onSave st ev = do
  f' <- handleFormEvent ev $ st^.form
  if st^.editMode then do
    M.continue $ st & (notes . noteData . ix (st^.selectedIndex)) .~ Note{_title = formState f'^.title,_content = formState f'^.content,_selected = False,_highlighted=formState f'^.highlighted}
                    & showDialog    .~ False
                    & editMode      .~ False
                    & form          .~ emptyForm
                    & selectedIndex .~ (-1)
  else do
    M.continue $ st & (notes . noteData) %~ (++[Note{_title = formState f'^.title,_content = formState f'^.content,_selected = False,_highlighted = formState f'^.highlighted}])
                    & showDialog .~ False
                    & form .~ emptyForm
                    & (notes . totalNotes) %~ (+1)

scroll st by = do
  let vp = viewportScroll MainViewPort
  vScrollBy vp by
  M.continue st

edit st ev = do
  if st^.showDialog then do
    onFormUpdate st ev
  else do
    M.continue $ st & form .~ setForm (st^.(notes . noteData) ^? ix (st^.selectedIndex))
                    & showDialog  .~ True
                    & editMode    .~ True

select st by = do
  let nextSelIndex = moveSelect by (L.length (st^.(notes . noteData))) (st^.selectedIndex)
  M.continue $ st & (notes . noteData . ix (st^.selectedIndex) . selected)  %~ not
                  & selectedIndex .~ nextSelIndex
                  & (notes . noteData . ix nextSelIndex  . selected)  %~ not

remove st = do
  M.continue $ st & (notes . noteData) .~ removeNote st
                  & (notes . noteData . each . selected) .~ False
                  & selectedIndex .~ (-1)
                  & (notes . totalNotes) %~ (\v -> v - 1)

onExit st = do
  if st^.persistFile == "" then do
    return st
  else do
    encodeFile (st^.persistFile) $ st^.notes.noteData & each . selected .~ False
    return st

onStart (FileInput path) = do
  contents <- readFile path
  let response = eitherDecode ((BL.fromStrict . B.pack) contents) :: Either String [Note]
  case response of
    Left e      -> return $ initApp [] path
    Right notes -> return $ initApp notes path
onStart _ = return $ initApp [] ""

cloneOrCancel st = do
  if st^.showDialog then do
    M.continue $ st & showDialog .~ False
  else do
    M.continue $ clone st

moveSelect :: Int -> Int -> Int -> Int
moveSelect d l i = (i + d) `mod` l

clone :: AppState e Name -> AppState e Name
clone st = case st^.(notes . noteData) ^? ix (st^.selectedIndex) of
            Nothing -> st
            Just n  -> st & (notes . noteData) %~ (++ [unselect n])

unselect :: Note -> Note
unselect n = n {_selected = False }

removeNote :: AppState e Name -> [Note]
removeNote st = fromMaybe [] newNotes
  where curNotes = st^. (notes . noteData)
        selectedNote = st^.(notes . noteData) ^? ix (st^.selectedIndex)
        newNotes = delete <$> selectedNote <*> Just curNotes

initApp :: [Note] -> FilePath -> AppState e Name
initApp notes path = AppState{
                            _notes = Notes {
                                _totalNotes = L.length notes,
                                _currentNote = 0,
                                _noteData = notes
                              },
                            _selectedIndex = -1,
                            _form = emptyForm,
                            _editMode = False,
                            _showDialog = False,
                            _persistFile = path,
                            _showHelp = True,
                            _dlg = getDialog
                          }
