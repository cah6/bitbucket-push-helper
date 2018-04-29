{-# LANGUAGE LambdaCase, DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, TemplateHaskell #-}
module Bitbucket where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON)
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

type BitbucketAPI = BasicAuth "foo-realm" () :> "rest" :> "api" :> "1.0" :> "projects" :> Capture "project" Text :> "repos" :> Capture "repo" Text :> ReqBody '[ JSON] CreatePullRequest :> Post '[ JSON] ()

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

instance ToJSON CreatePullRequest

defaultCreatePr :: CreatePullRequest
defaultCreatePr = CreatePullRequest
  { _title = ""
  , _description = ""
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

instance ToJSON BranchReference

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

instance ToJSON Repository

defaultRepository :: Repository
defaultRepository = Repository
  { _slug = ""
  , _project = defaultProject
  }

--------------------------------------
newtype Project = Project
  { _key :: Text
  } deriving (Generic, Show)

instance ToJSON Project

defaultProject :: Project
defaultProject = Project { _key = "" }

--------------------------------------
newtype Reviewer = Reviewer
  { _user :: User
  } deriving (Generic, Show)

instance ToJSON Reviewer

defaultReviewer :: Reviewer
defaultReviewer = Reviewer { _user = defaultUser }

--------------------------------------

newtype User = User
  { _name :: Text
  } deriving (Generic, Show)

instance ToJSON User

defaultUser :: User
defaultUser = User { _name = "" }

--------------------------------------

makeLenses ''CreatePullRequest
makeLenses ''BranchReference
makeLenses ''Repository
makeLenses ''Project
makeLenses ''Reviewer
makeLenses ''User
