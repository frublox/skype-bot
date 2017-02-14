{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Skype 
    ( AuthState
    , authToken
    , timeLeft
    , ClientCreds(..)
    , Skype
    , runSkype
    , withAuth
    )
where

import Skype.Types

import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T
import Data.IORef
import Data.Monoid ((<>))

import qualified Network.Wreq as Wreq
import Network.Wreq (FormParam(..))
import Network.Wreq.Lens

import Control.Lens
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class

data AuthState = AuthState 
    { _authToken :: Wreq.Auth
    , _timeLeft :: IORef Int }
makeLenses ''AuthState

newtype ClientCreds = ClientCreds 
    { unClientCreds :: (BL.ByteString, BL.ByteString) }
    deriving (Eq, Show)

type HasCreds m = ReaderT ClientCreds m
type HasAuth m = StateT AuthState m

newtype Skype a = Skype { unSkype :: HasCreds (HasAuth IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, 
        MonadReader ClientCreds, MonadState AuthState)

runSkype :: MonadIO io => Skype a -> ClientCreds -> io (Maybe a)
runSkype skype creds = do
    authState <- authenticate creds

    case authState of
        Nothing -> return Nothing
        Just authState' -> do
            let unwrappedCreds = runReaderT (unSkype skype) creds
            unwrappedState <- evalStateT unwrappedCreds authState'
            return $ liftIO (Just unwrappedState)

authenticate :: ClientCreds -> IO (Maybe AuthState)
authenticate creds = do
    let ClientCreds (cliId, cliSecret) = creds
    r <- Wreq.post url (params cliId cliSecret)

    let json = r ^. responseBody
    
    case extractStuff json of
        Nothing -> do
            putStrLn "Couldn't extract token/time left from response"
            return Nothing
        Just (token, timeLeft) -> do
            ref <- newIORef timeLeft
            countdown ref
            let ouath = Wreq.oauth2Bearer token
            return $ Just (AuthState ouath ref)

    where
        url = "https://login.microsoftonline.com/botframework.com/" 
            <> "oauth2/v2.0/token"
        
        scope :: BL.ByteString
        scope = "https://api.botframework.com/.default"

        params cliId cliSecret = 
            [ "grant_type" := ("client_credentials" :: BL.ByteString)
            , "client_id" := cliId
            , "client_secret" := cliSecret
            , "scope" := scope ]

        extractStuff :: BL.ByteString -> Maybe (BS.ByteString, Int)
        extractStuff json = do
            token <- json ^? key "access_token" . _String
            timeLeft <- json ^? key "expires_in" . _Integral
            return (T.encodeUtf8 token, timeLeft)

        countdown :: IORef Int -> IO ()
        countdown ref = do
            threadDelay (10^6)
            val <- readIORef ref
            case val of
                0 -> return ()
                _ -> do
                    modifyIORef' ref (\x -> x - 1)
                    countdown ref

withAuth :: (MonadReader ClientCreds m, MonadState AuthState m, 
    MonadIO m) 
    => (Wreq.Auth -> a) 
    -> m (Maybe a)
withAuth f = do
    token <- use authToken
    time <- use timeLeft >>= fmap liftIO readIORef

    case (time < 10) of
        True -> do
            creds <- ask
            authState' <- liftIO (authenticate creds)
            case authState' of
                Nothing -> return Nothing
                Just newAuthState -> do
                    put newAuthState
                    withAuth f
        False -> return $ Just (f token)