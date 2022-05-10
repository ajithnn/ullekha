module Form(getForm,emptyForm,setForm) where

import           Brick.Forms
import           Brick.Types
import           Brick.Widgets.Border (border, borderWithLabel, vBorder)
import           Brick.Widgets.Core
import           Types

getForm :: Form Note e Name -> Widget Name
getForm  =  border . padTop (Pad 1) . hLimit 50 . renderForm

setForm :: Maybe Note -> Form Note e Name
setForm Nothing  = emptyForm
setForm (Just n) = mkForm n

emptyForm = mkForm Note{
  _title="",
  _tasks = [],
  _mode = FreeNote,
  _selectedTaskIndex = -1,
  _content="",
  _selected=False,
  _checkBoxSelected = False,
  _highlighted=False
}

mkForm :: Note -> Form Note e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Title" @@=
                  editTextField title TitleField(Just 1)
               , label "Content" @@=
                  editTextField content ContentField Nothing
               , label "" @@=
                   checkboxField highlighted HighlightField "Important ?"
               ]




