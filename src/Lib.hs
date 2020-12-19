{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data CalcResponse = CalcResponse {
  operator :: String,
  arguments :: [Double],
  result :: Double,
  error :: Maybe String
}

$(deriveJSON defaultOptions ''CalcResponse)

type API = "add" :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] CalcResponse
      :<|> "sub" :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] CalcResponse

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = add
    :<|> sub

add :: Double -> Double -> Handler CalcResponse
add x y = return (CalcResponse "add" [x, y] (x + y) Nothing)

sub :: Double -> Double -> Handler CalcResponse
sub x y = return (CalcResponse "sub" [x, y] (x - y) Nothing)
