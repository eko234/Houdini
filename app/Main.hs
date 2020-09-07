{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.Vty

welcomeMessage = " :D welcome to renegade,\
                 \ we know GUI's are too much\
                 \ and plain terminal isn't any fun "

main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let line0 = string (defAttr ` withForeColor ` white) welcomeMessage 
      img = line0 -- <-> line1 <-> line0
      pic = picForImage img
  update vty pic
  e <- nextEvent vty
  print e
  shutdown vty
  print ("Last event was: " ++ show e)
  
