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

import Data.Text
import Data.Proxy
import Control.Monad.IO.Class
import GHC.Generics
import System.Environment (getEnv)
import Data.Maybe (fromMaybe)

import Types
import Database

type PublicAPI =
  "login" :> ReqBody '[FormUrlEncoded] LoginForm
          :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie]
                                    User)
  :<|> Raw

checkCreds :: Connection -> CookieSettings -> JWTSettings -> LoginForm
           -> Handler (Headers '[Header "Set-Cookie" SetCookie] User)
checkCreds con cookieSettings jwtSettings (LoginForm login password) = do
  maybeUser <- liftIO $ getUser con (login, password)
  maybeCookie <- maybe (return Nothing) makeCookie' maybeUser
  case maybeCookie of
    Nothing -> throwError err401
    (Just cookie) -> return $ addHeader cookie $ fromMaybe nullUser maybeUser
  where
    makeCookie' = liftIO . (makeSessionCookie cookieSettings jwtSettings)
checkCreds _ _ _ _ = throwError err401

public :: Connection -> CookieSettings -> JWTSettings -> Server PublicAPI
public con cs jwts = checkCreds con cs jwts
                  :<|> serveDirectoryWebApp "static/"
  
type ProtectedAPI =
  "postPhrase" :> QueryParam "phrase" Text
               :> Post '[JSON] Text

protected :: Connection -> AuthResult User -> Server ProtectedAPI
protected con (Authenticated user) = postPhraseHandler con user
protected _ _ = throwAll err401

postPhraseHandler :: Connection -> User -> Maybe Text -> Handler Text
postPhraseHandler con user (Just t) = return $ "Yuppie" <> (userLastname user)
postPhraseHandler _ _ Nothing = return "(("

type ApplicationAPI auth = PublicAPI
  :<|> (Auth auth User :> ProtectedAPI)

server :: Connection -> CookieSettings -> JWTSettings -> Server (ApplicationAPI auth)
server con cs jwt = public con cs jwt :<|> protected con

runServer :: IO ()
runServer = do
  pgPassword <- getEnv "PGPASSWORD"
  (Right con) <- makeConnection pgPassword -- ""
  key <- generateKey
  let jwtcfg = defaultJWTSettings key
      api = Proxy :: (Proxy (ApplicationAPI '[JWT]))
      cfg = defaultCookieSettings :. jwtcfg :. EmptyContext
  -- c <- makeSessionCookie defaultCookieSettings jwtcfg nullUser
  -- putStrLn $ show c
  run 8080 $ serveWithContext api cfg
             (server con defaultCookieSettings jwtcfg)
