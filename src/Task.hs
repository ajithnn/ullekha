module Task where

import           Brick.Widgets.Core
import           Brick.Widgets.Edit as E (getEditContents)
import           Data.List          as L (intercalate, map)
import           Data.Text          as T hiding (unlines)
import           Lens.Micro         ((^.))
import           Note
import           Types


getTask st selected = Task{
              _status = False,
              _task = T.pack $ L.intercalate "" $ E.getEditContents (st^.(notes . taskEdit)),
              _selectedTask = selected
              }

getTasks st = L.map renderTask (st^.notes . tempTodoNote  . tasks)

renderTask tsk = (rendered . str  . show) tsk
  where rendered  | tsk^.selectedTask = withAttr "taskHighlighted"
                  | otherwise = withAttr "normalTask"

emptyTask = Task{
    _status = False,
    _task = "",
    _selectedTask = False
}
