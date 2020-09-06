module Remote where


data Remote
  = Remote
    { action :: Action
    } deriving (Eq,Show)

data Action
  = Add { remoteUrl :: Maybe String, remoteName :: Maybe String}
  | Unset
  deriving (Eq,Show)
updateRemote remote l
  = case action remote of
      Add Nothing    Nothing -> Remote $ Add (Just l) Nothing
      Add remoteUrl_ Nothing -> Remote $ Add remoteUrl_ (Just l)

initialRemote = Remote Unset

check r
  = case action r of
      Add (Just _) (Just _) -> True 
      Add _ _ -> False
      
