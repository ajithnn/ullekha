module Dialog where


import           Brick.Types
import           Brick.Widgets.Center (center, centerAbout)
import           Brick.Widgets.Core
import           Brick.Widgets.Dialog as D
import           Form
import           Lens.Micro           ((%~), (&), (.~), (^.))
import           Types

getDialog :: D.Dialog Choice
getDialog = D.dialog (Just "Note") (Just (0,choices)) 45
  where choices = [("(CTRL-S) Save",Save),("(CtRL-C) Cancel",Cancel)]

noteDialog :: AppState e Name -> Widget Name
noteDialog st = padLeft (Pad 0) $ padRight Max $ padBottom Max $
  renderDialog nDialog $ getForm $ st^.form
    where nDialog = st^.dlg


