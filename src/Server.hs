{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Servant
import Servant.API
import Servant.Server
import Servant.Auth
import Servant.Auth.Server
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Network.Wai.Handler.Warp (run)
import Hasql.Connection (Connection)

import Data.Int
import Data.Text
import Data.Proxy
import Control.Monad.IO.Class
import GHC.Generics
import System.Environment (getEnv)
import Data.Maybe (fromMaybe)

import Types
import Database

type PublicAPI =
  "login" :> QueryParam "login" Text
          :> QueryParam "password" Text
          :> Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                   , Header "Set-Cookie" SetCookie]
                                    User)
  :<|> "static" :> Raw

checkCreds :: Connection -> CookieSettings -> JWTSettings
           -> Maybe Text -> Maybe Text
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie] User)
checkCreds con cookieSettings jwtSettings (Just login) (Just password) = do
  maybeUser <- liftIO $ getUser con (login, password)
  maybeCookie <- maybe (return Nothing) makeCookie' maybeUser
  case maybeCookie of
    Nothing -> throwError err401
    (Just cookie) -> return $ cookie $ fromMaybe nullUser maybeUser
  where
    makeCookie' = liftIO . (acceptLogin cookieSettings jwtSettings)
    --makeCookie' = liftIO . (makeSessionCookie cookieSettings jwtSettings)
checkCreds _ _ _ _ _ = throwError err401

public :: Connection -> CookieSettings -> JWTSettings -> Server PublicAPI
public con cs jwts = checkCreds con cs jwts
                  :<|> serveDirectoryWebApp "static/"
  
type ProtectedAPI =
       "postPhrase"        :> ReqBody '[JSON] PhraseReq
                           :> Post '[JSON] (Either RespError Int)

  :<|> "postWording"       :> ReqBody '[JSON] WordingReq
                           :> Post '[JSON] Wording

  :<|> "getPhrases"        :> QueryFlag "showMy"
                           :> QueryFlag "showApproved"
                           :> Get '[JSON] [Phrase]

  :<|> "getWordings"       :> Capture "phraseId" Int
                           :> Get '[JSON] (Phrase, [Wording])

  :<|> "toggleWording"    :> ReqBody '[JSON] WordingIdReq
                           :> Put '[JSON] Wording

protected :: Connection -> AuthResult User -> Server ProtectedAPI
protected con (Authenticated user) =
       postPhraseHandler con user
  :<|> postWordingHandler con user
  :<|> getPhrasesHandler con user
  :<|> getWordingsHandler con user
  :<|> toggleWordingHandler con user
protected _ _ = throwAll err401

postPhraseHandler :: Connection -> User -> PhraseReq -> Handler (Either RespError Int)
postPhraseHandler con user (PhraseReq text) = do
  eth <- liftIO $ insertPhrase con (toEnum $ userId user, text)
  case eth of
    (Left err) -> return $ Left $ RespError $ show err
    (Right amt) -> return $ Right $ fromIntegral amt

postWordingHandler :: Connection -> User -> WordingReq -> Handler Wording
postWordingHandler = undefined

getPhrasesHandler :: Connection -> User -> Bool -> Bool -> Handler [Phrase]
getPhrasesHandler = undefined

getWordingsHandler :: Connection -> User -> Int -> Handler (Phrase, [Wording])
getWordingsHandler = undefined

toggleWordingHandler :: Connection -> User -> WordingIdReq -> Handler Wording
toggleWordingHandler = undefined

type ApplicationAPI auth = (Auth auth User :> ProtectedAPI) :<|> PublicAPI

server :: Connection -> CookieSettings -> JWTSettings -> Server (ApplicationAPI auth)
server con cs jwt = protected con :<|> public con cs jwt

runServer :: IO ()
runServer = do
  pgPassword <- getEnv "PGPASSWORD"
  (Right con) <- makeConnection pgPassword -- ""
  key <- generateKey
  let jwtcfg = defaultJWTSettings key
      api = Proxy :: (Proxy (ApplicationAPI '[JWT, Cookie]))
      cfg = defaultCookieSettings :. jwtcfg :. EmptyContext
  run 8080 $ serveWithContext api cfg
             (server con defaultCookieSettings jwtcfg)
