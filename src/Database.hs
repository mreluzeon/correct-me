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

import Utils
import Types

makeConnection :: String -> IO (Either Con.ConnectionError Con.Connection)
makeConnection password =
  Con.acquire $ Con.settings "aws-0-eu-central-1.pooler.supabase.com"
                             6543
                             "postgres.xbwlsnnbbhunkrapboar"
                             (DBC.pack password)
                             "postgres"

userDecoder :: Dec.Result (Maybe User)
userDecoder =
  Dec.rowMaybe $ User
    <$> (fromIntegral
        <$> (Dec.column . Dec.nonNullable) Dec.int8)
    <*> (Dec.column . Dec.nonNullable) Dec.text
    <*> (Dec.column . Dec.nonNullable) Dec.text
    <*> (Dec.column . Dec.nonNullable) Dec.text

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

phrasesDecoder :: Dec.Row Phrase
phrasesDecoder = Phrase
    <$> (fromIntegral <$> (Dec.column . Dec.nonNullable) Dec.int8)
    <*> (Dec.column . Dec.nonNullable) Dec.text
    <*> (fromIntegral <$> (Dec.column . Dec.nonNullable) Dec.int8)
    <*> (Dec.column . Dec.nonNullable) Dec.timestamptz
    <*> (Dec.column . Dec.nonNullable) Dec.bool

wordingDecoder :: Dec.Row Wording
wordingDecoder = Wording
  <$> (fromIntegral <$> (Dec.column . Dec.nonNullable) Dec.int8)
  <*> (Dec.column . Dec.nonNullable) Dec.text
  <*> (fromIntegral <$> (Dec.column . Dec.nonNullable) Dec.int8)
  <*> (fromIntegral <$> (Dec.column . Dec.nonNullable) Dec.int8)
  <*> (Dec.column . Dec.nonNullable) Dec.bool
  <*> (Dec.column . Dec.nonNullable) Dec.timestamptz

getPhrasesStatement :: St.Statement (Int64, Bool, Bool) [Phrase]
getPhrasesStatement = St.Statement sql encoder (Dec.rowList phrasesDecoder) False
  where
    sql = "select phrase_id, author_id, phrase_text, creation_date, amt==0 \
           \ from ( \
           \   select *, (select count(*) \
           \              from public.\"Wordings\" t \
           \              where t.phrase_id = p.phrase_id \
           \                    and t.isApproved) amt \
           \   from public.\"Phrases\" p) s \
           \ where (amt != 0 or not $2) and (author_id = $1 or not $3)"
    encoder = (fst3 >$< Enc.param (Enc.nonNullable Enc.int8))
           <> (scd3 >$< Enc.param (Enc.nonNullable Enc.bool))
           <> (thd3 >$< Enc.param (Enc.nonNullable Enc.bool))

--(userId, getApproved, getMine) 
getPhrases :: Con.Connection -> (Int64, Bool, Bool) -> IO [Phrase]
getPhrases con params = do
  eitherPhrases <- Ses.run session con
  case eitherPhrases of
    (Left e) -> (putStrLn $ show e) >> return []
    (Right phrases) -> return phrases
  where
    session = Ses.statement params getPhrasesStatement

getPhraseStatement :: St.Statement Int64 (Maybe Phrase)
getPhraseStatement = St.Statement sql encoder (Dec.rowMaybe phrasesDecoder) False
  where
    sql = "select * \
           \ from ( \
           \   select *, (select count(*) \
           \              from public.\"Wordings\" t \
           \              where t.phrase_id = p.phrase_id \
           \                    and t.isApproved) amt \
           \   from public.\"Phrases\" p) s \
           \ where phrase_id = $1 "
    encoder = Enc.param (Enc.nonNullable Enc.int8)

getPhrase :: Con.Connection -> Int64 -> IO (Maybe Phrase)
getPhrase con param = do
  eitherPhrase <- Ses.run session con
  case eitherPhrase of
    (Left e) -> (putStrLn $ show e) >> return Nothing
    (Right phrase) -> return phrase
  where
    session = Ses.statement param getPhraseStatement

getWordingsStatement :: St.Statement Int64 [Wording]
getWordingsStatement = St.Statement sql encoder (Dec.rowList wordingDecoder) False
  where
    sql = "select * \
           \ from public.\"Wordings\" \
           \ where phrase_id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.int8)

getWordings :: Con.Connection -> Int64 -> IO [Wording]
getWordings con phrasid = do
  eitherWordings <- Ses.run session con
  case eitherWordings of
    (Left e) -> (putStrLn $ show e) >> return []
    (Right wordings) -> return wordings
  where
    session = Ses.statement phrasid getWordingsStatement

getWordingStatement :: St.Statement Int64 (Maybe Wording)
getWordingStatement = St.Statement sql encoder (Dec.rowMaybe wordingDecoder) False
  where
    sql = "select * \
           \ from public.\"Wordings\" \
           \ where wording_id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.int8)

getWording :: Con.Connection -> Int64 -> IO (Maybe Wording)
getWording con wordingId = do
  eitherWording <- Ses.run session con
  case eitherWording of
    (Left e) -> (putStrLn $ show e) >> return Nothing
    (Right wording) -> return wording
  where
    session = Ses.statement wordingId getWordingStatement

insertPhraseStatement :: St.Statement (Int64, Text) Int64
insertPhraseStatement = St.Statement sql encoder Dec.rowsAffected False
  where
    sql = "insert into public.\"Phrases\" \
           \ (author_id, phrase_text) \
           \ values ($1, $2)"
    encoder = (fst >$< Enc.param (Enc.nonNullable Enc.int8))
           <> (snd >$< Enc.param (Enc.nonNullable Enc.text))

insertPhrase :: Con.Connection -> (Int64, Text) -> IO (Either Ses.QueryError Int64)
insertPhrase con params = Ses.run session con
  where
    session = Ses.statement params insertPhraseStatement

insertWordingStatement :: St.Statement (Int64, Int64, Text) Int64
insertWordingStatement = St.Statement sql encoder Dec.rowsAffected False
  where
    sql = "insert into public.\"Wordings\" \
           \ (phrase_id, author_id, wording) \
           \ values ($1, $2, $3)"
    encoder = (fst3 >$< Enc.param (Enc.nonNullable Enc.int8))
           <> (scd3 >$< Enc.param (Enc.nonNullable Enc.int8))
           <> (thd3 >$< Enc.param (Enc.nonNullable Enc.text))

insertWording :: Con.Connection -> (Int64, Int64, Text) -> IO (Either Ses.QueryError Int64)
insertWording con params = Ses.run session con
  where
    session = Ses.statement params insertWordingStatement

toggleWordingStatement :: St.Statement (Int64, Int64) Int64
toggleWordingStatement = St.Statement sql encoder Dec.rowsAffected False
  where
    sql = "update public.\"Wordings\" w \
           \ set is_approved = not is_approved \
           \ where wording_id = $1 and \
           \    (select p.author_id \
           \     from public.\"Phrases\" p \
           \     where p.phrase_id = w.phrase_id limit 1) = $2"
    encoder = (fst >$< Enc.param (Enc.nonNullable Enc.int8))
           <> (snd >$< Enc.param (Enc.nonNullable Enc.int8))

toggleWording :: Con.Connection -> (Int64, Int64) -> IO (Either Ses.QueryError Int64)
toggleWording con params = Ses.run session con
  where
    session = Ses.statement params toggleWordingStatement
