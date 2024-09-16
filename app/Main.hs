{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Servant.API
import Data.Text
import GHC.Generics
import Servant.Server
import Servant
import Network.Wai.Handler.Warp
import Web.FormUrlEncoded
import Data.Functor.Contravariant

import Types
import Lib
import Database
import Server

main :: IO ()
main = runServer
