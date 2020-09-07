{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.Vty
import Lib


welcomeMessage = " :D welcome to renegade,\
                 \ we know GUI's are too much\
                 \ and plain terminal isn't any fun "


runApp vty model_ = do
  e <- nextEvent vty
  let pic = picForImage $ string (defAttr `withForeColor` white) "test"
  update vty pic
  runApp vty model_ 

main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let line0 = string (defAttr ` withForeColor ` white) welcomeMessage 
      img = line0 -- <-> line1 <-> line0
      pic = picForImage img
  update vty pic
  _ <- runApp vty initialModel
  shutdown vty
  print "bye bye baby"
  
