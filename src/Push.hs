module Push where


data Push
  = Push
    { target :: PushTarget
    , args :: PushArgs
    } deriving (Eq,Show)

data PushTarget
  = Remote
  | Upstream
  | Other
  | Unset
  deriving (Eq,Show)

data PushArgs
  = PushArgs
    { forceWithLease :: Bool
    , force :: Bool
    , disableHooks :: Bool
    , dryRun :: Bool
    } deriving (Eq,Show)

updatePush push l
  = case l of
      "p" -> push { target = Remote }

initialPush = Push Unset (PushArgs False False False False) 

check p
  = case target p of
    Unset -> False
    _     -> True
