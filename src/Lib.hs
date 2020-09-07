module Lib
    ( initialModel
    , updateModel
    ) where

import qualified Push as P 
import qualified Remote as R

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

data UI
  = UI
    { header :: String
    , message :: String
    , menu :: [String]
    , thatVimThing :: String
    } deriving (Eq,Show)

data Model
  = Model
    { command :: GitCommand 
    , status :: Status
    , ui :: UI 
    } deriving (Eq,Show)

initialModel = Model Unset Initial (UI [] [] [] [])

setCommand model l
  = case l of
      "P" -> model { command = Push P.initialPush }
      "M" -> model { command = Remote R.initialRemote }

updateCommand model l =
  case command model of
    Push b ->
      let model'@(Model (Push b') _ _) = model { command = Push (P.updatePush b l) }
      in if P.check b' then model' { status = Success } else model'
    Remote b -> 
      let model'@(Model (Remote b') _ _) = model { command = Remote (R.updateRemote b l) }
      in if R.check b' then model' { status = Success } else model'
    Unset -> model {status = Initial}


updateModel model l = 
  case status model of
    Initial ->  setCommand model l 
    Building -> updateCommand model l

      

