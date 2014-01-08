{--------------------------------------------------------------------------------
  The 'hello world' demo from the wxWindows site.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WXCore

main
  = run helloWorld

helloWorld
  = do -- create file menu
       fm <- menuCreate "" 0
       menuAppend fm wxID_ABOUT "&About.." "About wxHaskell" False {- not checkable -}
       menuAppendSeparator fm
       menuAppend fm wxID_EXIT "&Quit\tCtrl-Q"    "Quit the demo"  False

       -- create menu bar
       m  <- menuBarCreate 0
       menuBarAppend m fm "&File"

       -- create top frame
       f  <- frameCreate objectNull idAny "Hello world" rectZero frameDefaultStyle
       windowSetBackgroundColour f white
       windowSetClientSize f (sz 600 250)

       -- set status bar with 1 field
       frameCreateStatusBar f 1 0
       frameSetStatusText f "Welcome to wxHaskell" 0

       -- connect menu
       frameSetMenuBar f m
       evtHandlerOnMenuCommand f wxID_ABOUT (onAbout f)
       evtHandlerOnMenuCommand f wxID_EXIT  (onQuit f)

       -- show it
       windowShow f
       windowRaise f
       return ()
  where
    onAbout f
      = do version <- versionNumber
           messageDialog f "About 'Hello World'" ("This is a wxHaskell " ++ show version ++ " sample") (wxOK + wxICON_INFORMATION)
           return ()

    onQuit f
      = do windowClose f True {- force close -}
           return ()
