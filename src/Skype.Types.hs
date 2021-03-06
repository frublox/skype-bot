{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GenericNewtypeDeriving #-}

module Skype.Types where

import Control.Lens

import Data.Char (toLower)
import Data.Text (Text)

import Data.Aeson
import Data.Aeson.TH
import Data.String (IsString)

import Misc (modifyIndex)

newtype Id = Id Text
    deriving (Show, IsString, FromJSON, ToJSON)

data ContactAction = ContactAdd | ContactRemove
    deriving (Show)

instance FromJSON ContactAction where
    parseJSON (String str) =
        case str of
            "add" -> return ContactAdd
            "remove" -> return ContactRemove

    parseJSON invalid = typeMismatch "ContactAction" invalid

instance ToJSON ContactAction where
    toJSON ContactAdd = String "add"
    toJSON ContactRemove = String "Remove"

data Address = Address
    { _adId      :: Id
    , _adName    :: Text
    , _adIsGroup :: Maybe Bool
    } deriving (Eq, Show)
makeLenses ''Address

$(deriveJSON defaultOptions
    { fieldLabelModifier = modifyIndex 0 toLower . drop 3
    , omitNothingFields = True
    } ''Address)

data Activity = Activity
    { _acType :: Text
    , _acText :: Text
    } deriving (Eq, Show)
makeLenses ''Activity

initActivity :: Text -> Text -> Activity
initActivity activityType activityText = Activity
    { _acType = activityType
    , _acText = activityText
    }

$(deriveJSON defaultOptions
    { fieldLabelModifier = modifyIndex 0 toLower . drop 3
    } ''Activity)

-- This payload is a JSON object that is pushed to the bot whenever there is an update
data Payload = Payload
    { _plId               :: Id
    , _plFrom             :: Address
    , _plTo               :: Maybe Address -- is this even used...?
    , _plTimestamp        :: Text
    , _plRecipient        :: Address
    , _plChannelId        :: Text
    , _plServiceUrl       :: Text
    , _plType             :: Text
    , _plSummary          :: Maybe Text
    , _plText             :: Text
    , _plAttachments      :: [Object]
    , _plEntities         :: [Object]

    -- Contact Relation Update
    , _plContactAction    :: Maybe ContactAction

    -- Conversation Update
    , _plConversation     :: Maybe Address
    , _plTopicName        :: Maybe Text
    , _plHistoryDisclosed :: Maybe Bool
    , _plMembersAdded     :: Maybe [Text]
    , _plMembersRemoved   :: Maybe [Text]
    } deriving (Eq, Show)
makeLenses ''Payload

$(deriveJSON defaultOptions
    { fieldLabelModifier = modifyIndex 0 toLower . drop 3
    , omitNothingFields = True
    } ''Payload)
