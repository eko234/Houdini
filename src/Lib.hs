module Lib
    ( someFunc
    ) where

import qualified Push as P 
import qualified Remote as R

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data GitCommand
  = Push P.Push
  | Remote R.Remote
  | Unset
  deriving (Eq,Show)

data Status
  = Abort
  | Building 
  | Success
  | Initial
  deriving (Eq,Show)

data Model
  = Model
    { command :: GitCommand 
    , status :: Status
    } deriving (Show,Eq)

initialModel = Model Unset Initial

setCommand model l
  = case l of
      "P" -> model { command = Push P.initialPush }
      "M" -> model { command = Remote R.initialRemote }

updateCommand model l =
  case command model of
    Push b ->
      let model'@(Model (Push b') _) = model { command = Push (P.updatePush b l) }
      in if P.check b' then model' { status = Success } else model'
    Remote b -> 
      let model'@(Model (Remote b') _) = model { command = Remote (R.updateRemote b l) }
      in if R.check b' then model' { status = Success } else model'
    Unset -> model {status = Initial}


updateModel model l = 
  case status model of
    Initial ->  setCommand model l 
    Building -> updateCommand model l

      
gitI :: Model -> IO (Model)
gitI model = do
  l <- getLine
  let model' = updateModel model l 
  case status model' of
    Building -> gitI model'
    Success  -> return model'
    _ -> gitI initialModel 

t = do
  print " :D welcome to renegade,\
        \ we know GUI's are too much\
        \ and plain terminal isn't any fun "
        
  status <- gitI initialModel 
  print status
