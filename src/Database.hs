{-# LANGUAGE OverloadedStrings #-}

module Database where

import qualified Hasql.Encoders as Enc
import qualified Hasql.Decoders as Dec
import qualified Hasql.Session as Ses
import qualified Hasql.Statement as St
import qualified Hasql.Connection as Con

import Data.Text
import Data.Int
import Data.ByteString
import Data.Functor.Contravariant
import qualified Data.ByteString.Char8 as DBC

import Types

userDecoder :: Dec.Result (Maybe User)
userDecoder =
  Dec.rowMaybe $ User
    <$> (fromIntegral
        <$> (Dec.column . Dec.nonNullable) Dec.int8)
    <*> (Dec.column . Dec.nonNullable) Dec.text
    <*> (Dec.column . Dec.nonNullable) Dec.text
    <*> (Dec.column . Dec.nonNullable) Dec.text

makeConnection :: String -> IO (Either Con.ConnectionError Con.Connection)
makeConnection password =
  Con.acquire $ Con.settings "aws-0-eu-central-1.pooler.supabase.com"
                             6543
                             "postgres.xbwlsnnbbhunkrapboar"
                             (DBC.pack password)
                             "postgres"

getUserStatement :: St.Statement (Text, Text) (Maybe User)
getUserStatement = St.Statement sql encoder userDecoder False
  where
    sql = "select user_id, login, firstname, lastname \
           \ from public.\"Users\" \
           \ where login=$1 and password=encode(digest($2, 'sha256'), 'hex') \
           \ limit 1"
    encoder = (fst >$< Enc.param (Enc.nonNullable Enc.text))
           <> (snd >$< Enc.param (Enc.nonNullable Enc.text))

getUser :: Con.Connection -> (Text, Text) -> IO (Maybe User)
getUser con cred = do
  eitherUser <- Ses.run session con
  case eitherUser of
    (Left e) -> (putStrLn $ show e) >> return Nothing
    (Right user) -> return user
  where
    session = Ses.statement cred getUserStatement
