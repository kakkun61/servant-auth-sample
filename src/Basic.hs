{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Basic where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Text

newtype PrivateData = PrivateData { privateText :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

newtype PublicData = PublicData { someData :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

newtype User = User { userName :: Text }
  deriving (Eq, Show)

type PublicAPI = Get '[JSON] [PublicData]

type PrivateAPI = Get '[JSON] PrivateData

type BasicAPI = "public" :> PublicAPI
           :<|> "private" :> BasicAuth "foo-realm" User :> PrivateAPI

basicAuthApi :: Proxy BasicAPI
basicAuthApi = Proxy

authCheck :: BasicAuthCheck User
authCheck =
  let
    check (BasicAuthData username password) =
      if username == "servant" && password == "server"
        then return (Authorized (User "servant"))
        else return Unauthorized
  in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

basicAuthServer :: Server BasicAPI
basicAuthServer =
  let
    publicAPIHandler = return [PublicData "foo", PublicData "bar"]
    privateAPIHandler (user :: User) = return (PrivateData (userName user))
  in publicAPIHandler :<|> privateAPIHandler

basicAuthMain :: IO ()
basicAuthMain =
  run 8080
    (serveWithContext
      basicAuthApi
      basicAuthServerContext
      basicAuthServer
    )
