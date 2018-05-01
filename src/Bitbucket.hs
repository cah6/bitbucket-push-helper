{-# LANGUAGE LambdaCase, DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, TemplateHaskell #-}
module Bitbucket where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON,fieldLabelModifier,defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Servant.API ((:>), BasicAuth, Capture, JSON, Post, ReqBody)
import Servant.API.BasicAuth (BasicAuthData)
import Servant.Client (ClientM, client)
import GHC.Generics (Generic)

createPr :: BasicAuthData -> Text -> Text -> CreatePullRequest -> ClientM ()
createPr = client api

api :: Proxy BitbucketAPI
api = Proxy

type BitbucketAPI = BasicAuth "foo-realm" ()
  :> "rest" :> "api" :> "1.0" :> "projects" :> Capture "project" Text :> "repos" :> Capture "repo" Text :> "pull-requests"
  :> ReqBody '[JSON] CreatePullRequest :> Post '[JSON] ()

--------------------------------------

data CreatePullRequest = CreatePullRequest
  { _title :: Text
  , _description :: Text
  , _state :: Text
  , _open :: Bool
  , _closed :: Bool
  , _fromRef :: BranchReference
  , _toRef :: BranchReference
  , _locked :: Bool
  , _reviewers :: [Reviewer]
  } deriving (Generic, Show)

defaultCreatePr :: CreatePullRequest
defaultCreatePr = CreatePullRequest
  { _title = ""
  , _description = " "
  , _state = "OPEN"
  , _open = True
  , _closed = False
  , _fromRef = defaultBranchReference
  , _toRef = defaultBranchReference
  , _locked = False
  , _reviewers = []
  }

--------------------------------------

data BranchReference = BranchReference
  { _id :: Text
  , _repository :: Repository
  } deriving (Generic, Show)

defaultBranchReference :: BranchReference
defaultBranchReference = BranchReference
  { _id = "master"
  , _repository = defaultRepository
  }

--------------------------------------

data Repository = Repository
  { _slug :: Text
  , _project :: Project
  } deriving (Generic, Show)

defaultRepository :: Repository
defaultRepository = Repository
  { _slug = ""
  , _project = defaultProject
  }

--------------------------------------

newtype Project = Project
  { _key :: Text
  } deriving (Generic, Show)

defaultProject :: Project
defaultProject = Project { _key = "" }

--------------------------------------

newtype Reviewer = Reviewer
  { _user :: User
  } deriving (Generic, Show)

defaultReviewer :: Reviewer
defaultReviewer = Reviewer { _user = defaultUser }

--------------------------------------

newtype User = User
  { _name :: Text
  } deriving (Generic, Show)

defaultUser :: User
defaultUser = User { _name = "" }

-- Make all the lenses

makeLenses ''CreatePullRequest
makeLenses ''BranchReference
makeLenses ''Repository
makeLenses ''Project
makeLenses ''Reviewer
makeLenses ''User

-- Make all the JSON instances

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CreatePullRequest
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BranchReference
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Repository
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Project
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Reviewer
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''User
