#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
{-# LANGUAGE LambdaCase, DeriveGeneric, OverloadedStrings,
  DataKinds, TypeOperators #-}

import Control.Applicative (empty)
import Control.Foldl (head)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text
       (Text(), isPrefixOf, replace, stripPrefix, unpack, unwords)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
       ((:<|>), (:>), BasicAuth, Capture, JSON, Post, ReqBody)
import Servant.API.BasicAuth (BasicAuthData(..))
import Servant.Client
       (BaseUrl(..), ClientM, Scheme(Https), client, mkClientEnv,
        runClientM)
import Turtle.Line (Line, lineToText)
import Turtle.Prelude (die, inshell, need, shell, shells)
import Turtle.Shell (Shell, fold, sh)

import Prelude hiding (head)
import GHC.Generics

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  userName <- getPropOrDie "OKTA_USERNAME" "Set it to be something like firstname.lastname@appdynamics.com"
  password <- getPropOrDie "OKTA_PASSWORD" "Set it to be whatever your password is!"
  branchName <- getCurrentBranchName
  shells (mkPushCommand branchName) empty
  let authData = BasicAuthData (encodeUtf8 userName) (encodeUtf8 password)
  res <- runClientM
      (createPr authData "Analytics" "analytics" mkPullRequest)
      (mkClientEnv manager
         (BaseUrl Https "bitbucket.corp.appdynamics.com" 443 ""))
  case res of
    Left err -> putStrLn $ "Error while creating PR: " ++ show err
    Right () -> putStrLn "Success!"

getPropOrDie :: Text -> Text -> IO Text
getPropOrDie prop message = need prop >>= \case
    Nothing -> die (prop <> " was not set. " <> message)
    Just a -> return a

getCurrentBranchName :: IO Text
getCurrentBranchName = fold (inshell "git branch | grep '*'" empty) head >>= \case
  Nothing -> die "No current branch. Are you in the right directory?"
  Just a  -> return $ lineToText a

mkPushCommand :: Text -> Text
mkPushCommand = (<>) "git push -u origin"

--------------------------------------
createPr :: BasicAuthData -> Text -> Text -> CreatePullRequest -> ClientM ()
createPr = client api

api :: Proxy BitbucketAPI
api = Proxy

type BitbucketAPI
   = BasicAuth "foo-realm" () :> "rest" :> "api" :> "1.0" :> "projects" :> Capture "project" Text :> "repos" :> Capture "repo" Text :> ReqBody '[ JSON] CreatePullRequest :> Post '[ JSON] ()

--------------------------------------
data CreatePullRequest = CreatePullRequest
  { title :: Text
  , description :: Text
  , state :: Text
  , open :: Bool
  , closed :: Bool
  , fromRef :: BranchReference
  , toRef :: BranchReference
  , locked :: Bool
  , reviewers :: [Reviewer]
  } deriving (Generic)

instance ToJSON CreatePullRequest

mkPullRequest :: CreatePullRequest
mkPullRequest = CreatePullRequest
  { title = "test1"
  , description = "test desc"
  , state = "OPEN"
  , open = True
  , closed = False
  , fromRef = mkBranchReference "feature/christian-test-3"
  , toRef = mkBranchReference "master"
  , locked = False
  , reviewers = [mkReviewer "david.chu@appdynamics.com"]
  }

--------------------------------------
data BranchReference = BranchReference
  { id :: Text
  , repository :: Repository
  } deriving (Generic)

instance ToJSON BranchReference

mkBranchReference :: Text -> BranchReference
mkBranchReference id =
  BranchReference
  {Main.id = id, repository = mkRepository "analytics" "Analytics"}

--------------------------------------
data Repository = Repository
  { slug :: Text
  , project :: Project
  } deriving (Generic)

instance ToJSON Repository

mkRepository :: Text -> Text -> Repository
mkRepository repoName projectName =
  Repository {slug = repoName, project = mkProject projectName}

--------------------------------------
newtype Project = Project
  { key :: Text
  } deriving (Generic)

instance ToJSON Project

mkProject :: Text -> Project
mkProject projectName = Project {key = projectName}

--------------------------------------
newtype Reviewer = Reviewer
  { user :: User
  } deriving (Generic)

instance ToJSON Reviewer

mkReviewer :: Text -> Reviewer
mkReviewer name = Reviewer {user = mkUser name}

--------------------------------------
newtype User = User
  { name :: Text
  } deriving (Generic)

instance ToJSON User

mkUser :: Text -> User
mkUser name = User {name = name}
--{
--    "title": "test pull request",
--    "description": "testing bitbucket api",
--    "state": "OPEN",
--    "open": true,
--    "closed": false,
--    "fromRef": {
--        "id": "feature/christian-test-3",
--        "repository": {
--            "slug": "analytics",
--            "name": null,
--            "project": {
--                "key": "Analytics"
--            }
--        }
--    },
--    "toRef": {
--        "id": "master",
--        "repository": {
--            "slug": "analytics",
--            "name": null,
--            "project": {
--                "key": "Analytics"
--            }
--        }
--    },
--    "locked": false,
--    "reviewers": [
--        {
--            "user": {
--                "name": "david.chu@appdynamics.com"
--            }
--        }
--    ],
--    "links": {
--        "self": [
--            null
--        ]
--    }
--}
