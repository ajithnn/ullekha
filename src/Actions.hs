module Actions (onSave,onFormUpdate) where

import           Brick
import           Brick.Forms
import qualified Brick.Main  as M
import           Brick.Types
import           Form
import           Lens.Micro  ((%~), (&), (.~), (^.))
import           Types       as Ty


onFormUpdate st ev = do
  f' <- handleFormEvent ev $ st^.form
  M.continue $ st & form .~ f'

onSave st ev = do
  f' <- handleFormEvent ev $ st^.form
  M.continue $ st & (notes . noteData) %~ (++[Note{_title = formState f'^.title,_content = formState f'^.content}])
                  & showDialog .~ False
                  & form .~ emptyForm
                  & (notes . totalNotes) %~ (+1)


