# Ullekha


A TUI for a notes app. Add notes , todos , random text anything directly on the terminal. Provide a filepath to be able to persist notes across terminal sessions. 
The app provides a completely mouse-free , keyboard-only interface to add / edit / delete / highlight any number of notes. The viewport is scrollable and can adjust automatically to terminal resizing. 


**Built using Haskell & Brick Library : https://hackage.haskell.org/package/brick-0.62** 

## Demo

https://user-images.githubusercontent.com/6824632/166922234-f281b485-977f-4d8e-9adc-9fd7d17e3e07.mov

## Features 

- Add any number of notes 
- Navigate the notes and delete selected notes 
- Edit any note
- Mark a note as important and it will be highlighted
- Clone a note and then edit as required
- Auto adjust layout upon resizing
- Persistence through a text file. 
- Paste into the note
- Add Todo support 
  - Add Tasks
  - Delete Tasks
  - Edit Tasks


## In Development 

- [X] Add Highlight to Todos
- [X] Allow persistence across devices 
	- Redis mode for persistence supported. Type ullekha --help for more information.
- [ ] Multi-select and delete 


## To be added (May be) 

- [ ] Timed notes which expire 
- [ ] Allow user to override keyboard shortcuts 

