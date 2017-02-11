{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T
import Data.IORef

import qualified Network.Wreq as Wreq
import Network.Wreq (FormParam(..))
import Network.Wreq.Lens

import Control.Lens
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class

data AuthState = AuthState { _authToken :: Wreq.Auth, _timeLeft :: IORef Int }
makeLenses ''AuthState

type ClientCreds = (BL.ByteString, BL.ByteString)

type HasCreds m = ReaderT ClientCreds m
type HasAuth m = StateT AuthState m

newtype Skype a = Skype { unSkype :: HasCreds (HasAuth IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader ClientCreds, MonadState AuthState)

runSkype :: Skype a -> ClientCreds -> IO (Maybe a)
runSkype skype creds = do
    authState <- authenticate creds

    case authState of
        Nothing -> return Nothing
        Just authState' -> do
            let unwrappedCreds = runReaderT (unSkype skype) creds
            unwrappedState <- evalStateT unwrappedCreds authState'
            return (Just unwrappedState)

authenticate :: ClientCreds -> IO (Maybe AuthState)
authenticate creds = do
    let (cliId, cliSecret) = creds
    r <- Wreq.post url (params cliId cliSecret)

    let json = r ^. responseBody
    
    case extractStuff json of
        Nothing -> do
            putStrLn "Couldn't extract token/time left from response"
            return Nothing
        Just (token, timeLeft) -> do
            ref <- newIORef timeLeft
            countdown ref
            return $ Just $ AuthState (Wreq.oauth2Bearer token) ref

    where
        url = "https://login.microsoftonline.com/botframework.com/oauth2/v2.0/token"

        params cliId cliSecret = 
            [ "grant_type" := ("client_credentials" :: BL.ByteString)
            , "client_id" := cliId
            , "client_secret" := cliSecret
            , "scope" := ("https://api.botframework.com/.default" :: BL.ByteString) ]

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

withAuth :: (MonadReader ClientCreds m, MonadState AuthState m, MonadIO m) 
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
                Just (AuthState token' _) -> return $ Just (f token')
        False -> return $ Just (f token)