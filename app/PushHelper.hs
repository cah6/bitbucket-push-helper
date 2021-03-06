#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
{-# LANGUAGE LambdaCase, DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, RecordWildCards #-}

import Control.Applicative (empty)
import Control.Foldl (head)
import Control.Lens ((.~))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecode, toJSON,encode)
import Data.ByteString.Lazy (readFile)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text
       (Text(), drop, isPrefixOf, replace, stripPrefix, unpack, unwords)
import Data.Text.Encoding (encodeUtf8)
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (encodeString)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API ((:<|>), (:>), BasicAuth, Capture, JSON, Post, ReqBody)
import Servant.API.BasicAuth (BasicAuthData(..))
import Servant.Client (BaseUrl(..), ClientM, Scheme(Https), client, mkClientEnv, runClientM)
import Turtle.Line (Line, lineToText)
import Turtle.Options (Parser, optPath, options)
import Turtle.Prelude (die, inshell, need, shell, shells)
import Turtle.Shell (Shell, fold, sh)

import Prelude hiding (head, drop, readFile, FilePath)
import GHC.Generics

import Bitbucket as BB

main :: IO ()
main = do
  args <- options "Script to create Bitbucket pull request with team specific defaults." optionsParser
  config <- readConfig (configPath args)
  userName <- getPropOrDie "OKTA_USERNAME" "Set it to be something like firstname.lastname@appdynamics.com"
  password <- getPropOrDie "OKTA_PASSWORD" "Set it to be whatever your okta password is!"
  localBranchName <- getLocalBranchName
  lastTitle <- getLastCommitTitle

  -- now all config related dependencies are satisfied
  shells (mkPushCommand localBranchName) empty
  manager <- newManager tlsManagerSettings
  let authData = BasicAuthData (encodeUtf8 userName) (encodeUtf8 password)
  let requestData = mkRequestData config lastTitle localBranchName
  res <- runClientM
      (BB.createPr authData (Main.project config) (Main.repository config) requestData)
      (mkClientEnv manager
         (BaseUrl Https "bitbucket.corp.appdynamics.com" 443 ""))
  case res of
    Left err -> putStrLn $ "Error while creating PR: " ++ show err
    Right () -> putStrLn "Success!"

optionsParser :: Parser ProgramArgs
optionsParser = ProgramArgs
  <$> optPath "config" 'c' "Location of config file that defines defaults for your team."

data ProgramArgs = ProgramArgs {
    configPath :: FilePath
  }

getPropOrDie :: Text -> Text -> IO Text
getPropOrDie prop message = need prop >>= \case
  Nothing -> die (prop <> " was not set. " <> message)
  Just a -> return a

getLocalBranchName :: IO Text
getLocalBranchName = fold (inshell "git branch | grep '*'" empty) head >>= \case
  Nothing -> die "No current branch. Are you in the right directory?"
  Just a  -> return $ drop 2 $ lineToText a -- drop 2 to trim off "* "

getLastCommitTitle :: IO Text
getLastCommitTitle = fold (inshell "git log --oneline --format=%B -n 1 HEAD | head -n 1" empty) head >>= \case
  Nothing -> die "Somehow there was no last git commit message?"
  Just a  -> return $ lineToText a

mkPushCommand :: Text -> Text
mkPushCommand = (<>) "git push -u origin "

readConfig :: FilePath -> IO Config
readConfig fp = do
  configInBytes <- readFile (encodeString fp)
  case eitherDecode configInBytes of
    Left _    -> die $ fromString "Could not read config file. Is your config file formatted as correct JSON?"
    Right val -> return val

mkRequestData :: Config -> Text -> Text -> BB.CreatePullRequest
mkRequestData Config{..} title localBranchName = BB.defaultCreatePr
  & BB.fromRef . BB.id .~ localBranchName
  & BB.fromRef . BB.repository . BB.slug .~ repository
  & BB.fromRef . BB.repository . BB.project . BB.key  .~ project
  & BB.toRef . BB.repository . BB.slug .~ repository
  & BB.toRef . BB.repository . BB.project . BB.key  .~ project
  & BB.reviewers .~ makeReviewers reviewers
  & BB.title .~ title

makeReviewers :: [Text] -> [Reviewer]
makeReviewers = map (\text -> BB.defaultReviewer & BB.user . BB.name .~ text)

--------------------------------------

data Config = Config
  { project :: Text
  , repository :: Text
  , reviewers :: [Text]
  } deriving (Generic, Show)

instance FromJSON Config
