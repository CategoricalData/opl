module Main where
import Graphics.UI.WX

main :: IO ()
main
  = start hello

hello :: IO ()
hello
  = do f    <- frame    [text := "Hello!"]
       quit <- button f [text := "Quit", on command := close f]
       set f [layout := widget quit]
