module Actions (
  onSave,
  onFormUpdate,
  scroll,
  onExit,
  onStart,
  remove,
  handleEnter,
  handleTypingEvents,
  handleTab,
  cloneOrCancel,
  select,
  initApp
  ) where

import           Brick
import           Brick.Focus
import           Brick.Forms
import qualified Brick.Main            as M
import           Brick.Types
import           Brick.Widgets.Edit    as E
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy  as BL hiding (readFile)
import           Data.List             as L (delete, elem, head, length)
import           Data.Map              as M
import           Data.Maybe
import           Data.Text             hiding (elem, unlines)
import           Dialog
import           Form
import qualified Graphics.Vty          as V
import           Lens.Micro            (each, ix, non, (%~), (&), (.~), (^.),
                                        (^?))
import           Note
import           System.Directory
import           Task
import           Types                 as Ty


onFormUpdate st ev = do
  if st^.showDialog then do
    f' <- handleFormEvent ev $ st^.form
    M.continue $ st & form .~ f'
  else do M.continue st

onSave st ev = do
  case st^.dialogMode of
    NoteCreate -> do
      f' <- handleFormEvent ev $ st^.form
      M.continue $ st & (notes . noteData) %~ (++[getFreeNote f' st])
                      & showDialog .~ False
                      & dialogMode    .~ ChoiceCreate
                      & dialogSelect  .~ NoteCreate
                      & form .~ emptyForm
                      & (notes . totalNotes) %~ (+1)
    TodoCreate -> do
      M.continue $ st & (notes . noteData) %~ (++[createTodoNote st])
                      & showDialog    .~ False
                      & resetTodo
                      & (notes . totalNotes) %~ (+1)
                      & selectedIndex .~ (-1)
    NoteEdit -> do
      f' <- handleFormEvent ev $ st^.form
      M.continue $ st & (notes . noteData . ix (st^.selectedIndex)) .~ getFreeNote f' st
                      & showDialog    .~ False
                      & dialogMode    .~ ChoiceCreate
                      & dialogSelect  .~ NoteCreate
                      & editMode      .~ False
                      & form          .~ emptyForm
                      & selectedIndex .~ (-1)
    _ -> M.continue st

scroll st by = do
  let vp = viewportScroll "MainViewPort"
  vScrollBy vp by
  M.continue st

handleTypingEvents st ev e = do
  case st^.dialogMode of
    ChoiceCreate -> do
      if st^.showDialog then do
        M.continue $ st & dialogMode .~ (st^.dialogSelect)
      else do
        M.continue st
    TodoCreate -> do
      if st^.showDialog then do
          M.continue =<< case focusGetCurrent (st^.notes.focusEdit) of
                            Just v -> case v of
                                        "TaskEdit"  -> handleEventLensed st (notes . taskEdit) E.handleEditorEvent e
                                        "TaskTitle" -> handleEventLensed st (notes . taskTitle) E.handleEditorEvent e
                                        "Tasks"     -> return $ handleTaskEdit st e
                                        _ -> return st
                            Nothing ->  return st
      else do
        M.continue st
    NoteCreate  -> onFormUpdate st ev
    NoteEdit    -> onFormUpdate st ev
    _ -> M.continue st

handleTaskEdit st (V.EvKey (V.KChar ' ') []) = st & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex) . status) %~ not
handleTaskEdit st (V.EvKey V.KDel [])        = st & (notes . tempTodoNote . tasks) .~ removeTask st
                                                  & (notes . focusEdit) .~ focusSetCurrent "TaskTitle" (focusRing ["TaskTitle","TaskEdit"])
                                                  & toggleSelectedIndex False (\_ -> -1)
                                                  & handleFocus
handleTaskEdit st _ = st

handleEnter st ev = do
  case st^.dialogMode of
    ChoiceCreate -> do
      if st^.showDialog then do
        M.continue $ st & dialogMode .~ (st^.dialogSelect)
      else do
          M.continue $ st & dialogMode .~ getDialogMode st
                          & form .~ setForm (st^.(notes . noteData) ^? ix (st^.selectedIndex))
                          & showDialog  .~ True
                          & editMode    .~ True
    TodoCreate -> M.continue $ st & (notes . tempTodoNote . tasks) %~ (++[getTask st False])
                                  & (notes . taskEdit)  .~ emptyEditor "TaskEdit"
    NoteCreate  -> onFormUpdate st ev
    NoteEdit    -> onFormUpdate st ev
    _ -> M.continue st

handleTab st ev = do
  case st^.dialogMode of
    ChoiceCreate -> do
      if (st^.dialogSelect) == NoteCreate then do
        M.continue $ st & dialogSelect .~ TodoCreate
      else do
        M.continue $ st & dialogSelect .~ NoteCreate
    TodoCreate -> M.continue $ handleFocus st
    NoteCreate -> onFormUpdate st ev
    NoteEdit   -> onFormUpdate st ev
    _ -> M.continue st

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
  exists <- doesFileExist path
  if exists then do
    contents <- readFile path
    let response = eitherDecode ((BL.fromStrict . B.pack) contents) :: Either String [Note]
    case response of
      Left e      -> return $ initApp [] path
      Right notes -> return $ initApp notes path
  else do return $ initApp [] path
onStart _ = return $ initApp [] ""

cloneOrCancel st = do
  if st^.showDialog then do
    M.continue $ st & showDialog    .~ False
                    & dialogMode    .~ ChoiceCreate
                    & dialogSelect  .~ NoteCreate
  else do
    M.continue $ clone st

clone :: AppState e Text -> AppState e Text
clone st = case st^.(notes . noteData) ^? ix (st^.selectedIndex) of
            Nothing -> st
            Just n  -> st & (notes . noteData) %~ (++ [unselect n])

removeNote :: AppState e Text -> [Note]
removeNote st = fromMaybe curNotes newNotes
  where curNotes = st^. (notes . noteData)
        selectedNote = st^.(notes . noteData) ^? ix (st^.selectedIndex)
        newNotes = L.delete <$> selectedNote <*> Just curNotes

removeTask :: AppState e Text -> [Task]
removeTask st = fromMaybe curTasks newTasks
  where curTasks = st^. (notes . tempTodoNote . tasks)
        selectedTask = st^.(notes . tempTodoNote . tasks) ^? ix (st^.notes.tempTodoNote.selectedTaskIndex)
        newTasks = L.delete <$> selectedTask <*> Just curTasks

moveSelect d l i = (i + d) `mod` l
unselect n = n {_selected = False }

toggleTaskSelected sel st = st & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex) . selectedTask) .~ sel
toggleSelectedIndex sel by st =  st & (notes . tempTodoNote .selectedTaskIndex) %~ by
                                    & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex) . selectedTask) .~ sel

isTask st totalTasks curFocus     = curFocus `L.elem` ["TaskTitle","Tasks"] && totalTasks > 0 && (st^.notes.tempTodoNote.selectedTaskIndex) < totalTasks-1
isLastTask st totalTasks curFocus = curFocus `L.elem` ["TaskTitle","Tasks"] && totalTasks > 0 && (st^.notes.tempTodoNote.selectedTaskIndex) == totalTasks-1

resetTodo st = st & dialogMode    .~ ChoiceCreate
                  & dialogSelect  .~ NoteCreate
                  & (notes . focusEdit) .~ focusRing ["TaskTitle","TaskEdit"]
                  & (notes . taskTitle) .~ emptyEditor "TaskTitle"
                  & (notes . taskEdit) .~ emptyEditor "TaskEdit"
                  & (notes . tempTodoNote)  .~ getTodoNote "" []

handleFocus st = nextFocus
  where curFocus = fromMaybe "TaskTitle" $ focusGetCurrent (st^.notes.focusEdit)
        totalTasks = L.length (st^.notes.tempTodoNote.tasks)
        nextFocus | isTask st totalTasks curFocus = st & (notes . focusEdit) .~ focusSetCurrent "Tasks" (focusRing ["Tasks"])
                                                       & toggleSelectedIndex False (+1)
                                                       & toggleTaskSelected True
                  | isLastTask st totalTasks curFocus  = st & (notes . focusEdit) .~ focusSetCurrent "TaskTitle" (focusRing ["TaskTitle","TaskEdit"])
                                                            & toggleSelectedIndex False (\_ -> -1)
                                                            & (notes . focusEdit) %~ focusNext
                  | otherwise = st  & toggleSelectedIndex False (\_ -> -1)
                                    & (notes . focusEdit) %~ focusNext

getDialogMode st = dlgMode
  where modes = M.fromList [(FreeNote,NoteEdit),(TodoList,TodoEdit)]
        dlgMode = M.findWithDefault ChoiceCreate (st^.(notes . noteData) ^? ix (st^.selectedIndex) ^. (non emptyNote . mode)) modes

initApp :: [Note] -> FilePath -> AppState e Text
initApp notes path = AppState{
                            _notes = initNotes notes,
                            _selectedIndex = -1,
                            _form = emptyForm,
                            _editMode = False,
                            _showDialog = False,
                            _dialogMode = ChoiceCreate,
                            _dialogSelect = NoteCreate,
                            _persistFile = path,
                            _showHelp = True,
                            _dlg = getDialog
                          }
