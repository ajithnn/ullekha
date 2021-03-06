module Actions (
  onSave,
  onFormUpdate,
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

import           Brick.Focus
import           Brick.Forms
import qualified Brick.Main             as M (continue)
import           Brick.Types
import           Brick.Widgets.Edit     as E
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson             as A (decode, eitherDecode, encode,
                                              encodeFile)
import qualified Data.ByteString.Char8  as B (pack)
import           Data.ByteString.Lazy   as BL (fromStrict, toStrict)
import           Data.List              as L (delete, elem, length)
import           Data.Map               as M (findWithDefault, fromList)
import           Data.Maybe             (fromMaybe, isJust, isNothing)
import           Data.Text              as T hiding (elem, unlines)
import           Data.Text.Encoding     (encodeUtf8)
import           Database.Redis         hiding (None, select)
import           Dialog
import           Form
import qualified Graphics.Vty           as V
import           Lens.Micro             (each, ix, non, (%~), (&), (.~), (^.),
                                         (^?))
import           Note
import           System.Directory       (doesFileExist)
import           System.IO
import           Task
import           Types

onFormUpdate st ev
    | st^.showDialog = handleFormEvent ev (st^.form) >>= (\f' -> M.continue $ st & form .~ f')
    | otherwise = M.continue st

onSave st ev =
  case st^.dialogMode of
    NoteCreate -> handleFormEvent ev (st^.form) >>=
      (\f' -> return $
              st  & (notes . noteData) %~ (++[getFreeNote f' st False])
                  & resetDialog
                  & form .~ emptyForm
                  & (notes . totalNotes) %~ (+1))
    NoteEdit -> handleFormEvent ev (st^.form) >>=
      (\f' ->   return $
                st  & (notes . noteData . ix (st^.selectedIndex)) .~ getFreeNote f' st True
                    & resetDialog
                    & editMode  .~ False
                    & form      .~ emptyForm)
    TodoCreate -> return $
                  st  & (notes . noteData) %~ (++[createTodoNote st])
                      & resetDialog
                      & resetTodo
                      & (notes . totalNotes) %~ (+1)
    TodoEdit -> return $
                st  & (notes . noteData . ix (st^.selectedIndex)) .~ createTodoNote st
                    & resetDialog
                    & resetTodo
                    & editMode .~ False
    _ -> return st

handleTypingEvents st ev e =
  case st^.dialogMode of
    ChoiceCreate  | st^.showDialog -> M.continue $ st & dialogMode .~ (st^.dialogSelect)
                  | otherwise -> M.continue st
    TodoCreate    | st^.showDialog -> handleToDoEditEvents st ev e
                  | otherwise -> M.continue st
    TodoEdit      | st^.showDialog -> handleToDoEditEvents st ev e
                  | otherwise -> M.continue st
    NoteCreate    -> onFormUpdate st ev
    NoteEdit      -> onFormUpdate st ev

handleEnter st ev =
  case st^.dialogMode of
    ChoiceCreate | st^.showDialog -> M.continue $ st & dialogMode .~ (st^.dialogSelect)
                 | otherwise -> M.continue $ st  & dialogMode .~ getDialogMode st & setFormState
    TodoCreate   -> handleTaskEdit st
    TodoEdit     -> handleTaskEdit st
    NoteCreate   -> onFormUpdate st ev
    NoteEdit     -> onFormUpdate st ev

handleTab st ev =
  case st^.dialogMode of
    ChoiceCreate  | (st^.dialogSelect) == NoteCreate -> M.continue $ st & dialogSelect .~ TodoCreate
                  | otherwise -> M.continue $ st & dialogSelect .~ NoteCreate
    TodoCreate  -> M.continue $ handleFocus st
    TodoEdit    -> M.continue $ handleFocus st
    NoteCreate  -> onFormUpdate st ev
    NoteEdit    -> onFormUpdate st ev

select st by = M.continue $
                st  & (notes . noteData . ix (st^.selectedIndex) . selected)  %~ not
                    & selectedIndex .~ nextSelIndex
                    & (notes . noteData . ix nextSelIndex  . selected)  %~ not
  where nextSelIndex = moveSelect by (L.length (st^.(notes . noteData))) (st^.selectedIndex)

remove st | not (st^.showDialog) =
            M.continue $
            st  & (notes . noteData) .~ removeNote st
                & (notes . noteData . each . selected) .~ False
                & selectedIndex .~ (-1)
                & (notes . totalNotes) %~ subtract 1
          | otherwise = M.continue st

onExit st
  | st^.persistFile == "" && isNothing (st^.persistRedis) = return st
  | isJust (st^.persistRedis)  = writeNotestoRedis st
  | otherwise = do
      encodeFile (st^.persistFile) $ st^.notes.noteData & each . selected .~ False
      return st

onStart = initApp

cloneOrCancel st
  | st^.showDialog = M.continue $ resetDialog st
  | otherwise = M.continue $ clone st

clone st = case st^.(notes . noteData) ^? ix (st^.selectedIndex) of
            Nothing -> st
            Just n  -> st & (notes . noteData) %~ (++ [unselect n])

removeNote st = removeItem (st^. (notes . noteData)) (st^.(notes . noteData) ^? ix (st^.selectedIndex))
removeTask st = removeItem (st^. (notes . tempTodoNote . tasks)) (st^.(notes . tempTodoNote . tasks) ^? ix (st^.notes.tempTodoNote.selectedTaskIndex))

removeItem curList selItem = fromMaybe curList newList
  where newList = L.delete <$> selItem <*> Just curList

moveSelect d l i = (i + d) `mod` l
unselect n = n {_selected = False }

setFormState st = newState  & showDialog .~ True
                            & editMode .~ True
  where newState  | st^.dialogMode == NoteEdit = st & form .~ setForm (st^.(notes . noteData) ^? ix (st^.selectedIndex))
                  | st^.dialogMode == TodoEdit = st & (notes . tempTodoNote) .~ (st^.notes.noteData ^? ix (st^.selectedIndex) ^. non emptyNote)
                                                    & (notes . taskTitle) .~ editor TaskTitle Nothing (T.unpack (st^.notes.noteData ^? ix (st^.selectedIndex) ^. (non emptyNote . title)))
                  | otherwise = st


resetTodo st = st & (notes . focusEdit) .~ focusRing [TaskTitle,TaskEdit,Checkbox]
                  & (notes . taskTitle) .~ emptyEditor TaskTitle
                  & (notes . taskEdit) .~ emptyEditor TaskEdit
                  & (notes . tempTodoNote)  .~ getTodoNote "" []

resetDialog st = st & dialogMode    .~ ChoiceCreate
                    & dialogSelect  .~ NoteCreate
                    & showDialog    .~ False

handleTaskEvents st (V.EvKey (V.KChar ' ') []) = st & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex) . status) %~ not
handleTaskEvents st (V.EvKey V.KDel [])        = st & (notes . tempTodoNote . tasks) .~ removeTask st
                                                    & (notes . focusEdit) .~ focusSetCurrent TaskTitle defaultTaskFocus
                                                    & updateSelectedTaskIndex False (\_ -> -1)
                                                    & handleFocus
handleTaskEvents st _ = st

handleToDoEditEvents st ev e = M.continue =<< case fromMaybe None $ focusGetCurrent (st^.notes.focusEdit) of
                                  TaskEdit  -> handleEventLensed st (notes . taskEdit) E.handleEditorEvent e
                                  TaskTitle -> handleEventLensed st (notes . taskTitle) E.handleEditorEvent e
                                  Tasks     -> return $ handleTaskEvents st e
                                  Checkbox  -> return $ st & (notes . tempTodoNote . highlighted) %~ not
                                  _ -> return st

handleFocus st = nextFocus
  where curFocus = fromMaybe TaskTitle $ focusGetCurrent (st^.notes.focusEdit)
        totalTasks = L.length (st^.notes.tempTodoNote.tasks)
        taskIndex = totalTasks -1
        selIndex = st^.notes.tempTodoNote.selectedTaskIndex
        canFocusTask curFocus totalTasks  = curFocus `L.elem` [TaskTitle,Tasks] && totalTasks > 0
        isTask index = index < taskIndex
        isLastTask index = index == taskIndex
        nextFocus | canFocusTask curFocus totalTasks && isTask selIndex = st  & (notes . focusEdit) .~ focusSetCurrent Tasks (focusRing [Tasks])
                                                                              & updateSelectedTaskIndex False (+1)
                                                                              & setTaskSelected
                  | canFocusTask curFocus totalTasks && isLastTask selIndex = st  & (notes . focusEdit) .~ focusSetCurrent TaskTitle defaultTaskFocus
                                                                                  & updateSelectedTaskIndex False (\_ -> -1)
                                                                                  & (notes . focusEdit) %~ focusNext
                  | curFocus == TaskEdit = st & notes . tempTodoNote . checkBoxSelected .~ True
                                                & (notes . focusEdit) %~ focusNext
                                                & updateSelectedTaskIndex False (\_ -> -1)
                  | curFocus == Checkbox = st & notes . tempTodoNote . checkBoxSelected .~ False
                                                & (notes . focusEdit) %~ focusNext
                  | otherwise = st  & (notes . focusEdit) %~ focusNext

handleTaskEdit st = case fromMaybe None $ focusGetCurrent (st^.notes.focusEdit) of
    TaskEdit  | st^.notes.taskEditMode -> M.continue $
                                            st  & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex)) .~ getTask st False
                                                & (notes . taskEdit)  .~ emptyEditor TaskEdit
                                                & updateSelectedTaskIndex False (\_ -> -1)
                                                & (notes . taskEditLabel) .~ "New Task"
                                                & (notes . taskEditMode)  .~ False
                | otherwise ->  M.continue $
                                st  & (notes . tempTodoNote . tasks) %~ (++[getTask st False])
                                    & (notes . taskEdit)  .~ emptyEditor TaskEdit
    Tasks -> M.continue $
                        st  & (notes . taskEditLabel) .~ "Edit Task"
                            & (notes . taskEdit)      .~ editor TaskEdit Nothing (getSelectedTaskContent st)
                            & (notes . focusEdit)     .~ focusSetCurrent TaskEdit (focusRing [TaskTitle,Tasks,TaskEdit,Checkbox])
                            & (notes . taskEditMode)  .~ True
    _ -> M.continue st

setTaskSelected st = st & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex) . selectedTask) .~ True
updateSelectedTaskIndex sel by st =  st & (notes . tempTodoNote . selectedTaskIndex) %~ by
                                        & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex) . selectedTask) .~ sel

getSelectedTaskContent st = T.unpack $ st^.(notes . tempTodoNote . tasks) ^? ix (st^.notes.tempTodoNote.selectedTaskIndex) ^. (non emptyTask . task)

getDialogMode st = dlgMode
  where modes = M.fromList [(FreeNote,NoteEdit),(TodoList,TodoEdit)]
        dlgMode = M.findWithDefault ChoiceCreate (st^.(notes . noteData) ^? ix (st^.selectedIndex) ^. (non emptyNote . mode)) modes

unselectNotes st = st^.notes.noteData & each . selected .~ False

initApp options = do
  (notes,persist) <- decodeOptions options
  case persist of
    (PersistFile path)    -> return   $ blankAppState notes (Just path) Nothing ""
    (PersistRedis conn k) -> return $ blankAppState notes Nothing (Just conn) k
    _                     -> return   $ blankAppState [] Nothing Nothing ""

blankAppState :: [Note] -> Maybe FilePath -> Maybe ConnectInfo -> RedisKey -> AppState e Name
blankAppState notes path conn key = AppState{
                            _notes = initNotes notes,
                            _selectedIndex = -1,
                            _form = emptyForm,
                            _editMode = False,
                            _showDialog = False,
                            _dialogMode = ChoiceCreate,
                            _dialogSelect = NoteCreate,
                            _persistFile = fromMaybe "" path,
                            _persistRedis = conn,
                            _persistRedisKey = key,
                            _showHelp = True,
                            _dlg = getDialog
                          }

decodeOptions :: CmdOptions -> IO ([Note],NotesPersist)
decodeOptions CmdOptions{inputType = FileInput path} = getNotesFromFile path >>= (\notes -> return (notes,PersistFile path))
decodeOptions CmdOptions{inputType = RedisInput connStr,redisKey = key} = do
  case (parseConnectInfo . T.unpack) connStr of
    Left e         -> return ([],PersistNone)
    Right connInfo -> withCheckedConnect connInfo (getNotesFromRedis key) >>= (\notes -> return (notes,PersistRedis connInfo key))
decodeOptions _ = return ([],PersistNone)

getNotesFromFile :: FilePath -> IO [Note]
getNotesFromFile path = withFile path ReadMode (\handle -> do
      contents <- hGetContents' handle `catch` \e -> const (return "[]") (e :: IOException)
      case eitherDecode ((BL.fromStrict . B.pack) contents) :: Either String [Note] of
        Left e      -> return []
        Right notes -> return notes )

writeNotestoRedis :: AppState e Name -> IO (AppState e Name)
writeNotestoRedis st = case st^.persistRedis of
  Nothing -> return st
  Just connInfo -> withCheckedConnect connInfo (\conn -> runRedis conn $ do
    let notes = A.encode $ unselectNotes st
    set (encodeUtf8 $ st^.persistRedisKey) $ BL.toStrict notes
    return st )


getNotesFromRedis :: Text -> Connection -> IO [Note]
getNotesFromRedis key conn = runRedis conn $ do
  notesString <- get $ encodeUtf8 key
  case notesString of
    Right nStr -> do
      case A.decode (maybe "[]" BL.fromStrict nStr) :: Maybe [Note] of
        Just n  -> return n
        Nothing -> return []
    Left _ -> return []
