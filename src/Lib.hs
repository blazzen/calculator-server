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
  result :: Maybe Double,
  error :: Maybe String
}

$(deriveJSON defaultOptions ''CalcResponse)

type API = "add" :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] CalcResponse
      :<|> "sub" :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] CalcResponse
      :<|> "mul" :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] CalcResponse
      :<|> "div" :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] CalcResponse
      :<|> "sqrt" :> Capture "x" Double :> Get '[JSON] CalcResponse
      :<|> "pow" :> Capture "x" Double :> Capture "n" Int :> Get '[JSON] CalcResponse

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = add
    :<|> sub
    :<|> mul
    :<|> Lib.div
    :<|> Lib.sqrt
    :<|> pow

add :: Double -> Double -> Handler CalcResponse
add x y = return (CalcResponse "add" [x, y] (Just (x + y)) Nothing)

sub :: Double -> Double -> Handler CalcResponse
sub x y = return (CalcResponse "sub" [x, y] (Just (x - y)) Nothing)

mul :: Double -> Double -> Handler CalcResponse
mul x y = return (CalcResponse "mul" [x, y] (Just (x * y)) Nothing)

div :: Double -> Double -> Handler CalcResponse
div x 0.0 = return (CalcResponse "div" [x, 0.0] Nothing (Just "Division by zero"))
div x y = return (CalcResponse "div" [x, y] (Just (x / y)) Nothing)

sqrt :: Double -> Handler CalcResponse
sqrt x
  | x < 0 = return (CalcResponse "sqrt" [x] Nothing (Just "Square root of negative number is a complex number"))
  | otherwise = return (CalcResponse "sqrt" [x] (Just (Prelude.sqrt x)) Nothing)

pow :: Double -> Int -> Handler CalcResponse
pow x n
  | x == 0 && n < 0 = return (CalcResponse "pow" [x, fromIntegral n] Nothing (Just "Negative power of zero leads to division by zero"))
  | otherwise = return (CalcResponse "pow" [x, fromIntegral n] (Just (x ** fromIntegral n)) Nothing)
