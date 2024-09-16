{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Time
import GHC.Generics
import Web.FormUrlEncoded (FromForm)
import Servant.Auth.Server

data LoginForm = LoginForm { login :: !Text, password :: !Text }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON, FromForm)

data User = User
  { userId :: Int
  , userLogin :: Text
  , userFirstname :: Text
  , userLastname :: Text
  } deriving (Eq, Show, ToJSON, FromJSON, Generic)

instance FromJWT User
instance ToJWT User

nullUser :: User
nullUser = User (-1) "" "" ""

data Phrase = Phrase
  { phraseId :: Int
  , phraseText :: Text
  , authorId :: Int
  , phraseCreationDate :: UTCTime
  , isPhraseApproved :: Bool
  } deriving (Eq, Show, ToJSON, FromJSON, Generic)

data Wording = Wording
  { wordingId :: Int
  , wordinText :: Text
  , wordingPhraseId :: Int
  , wordingAuthorId :: Int
  , isWordingApproved :: Bool
  , wordingCreationDate :: UTCTime
  } deriving (Eq, Show, ToJSON, FromJSON, Generic)


