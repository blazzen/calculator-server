{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /add/<x>/<y>" $ do
        it "add - happy case" $ do
            get "/add/1/2" `shouldRespondWith` "{\"operator\":\"add\",\"arguments\":[1.0,2.0],\"result\":3.0,\"error\":null}"
        it "add - happy case with negative number" $ do
            get "/add/1/-2.5" `shouldRespondWith` "{\"operator\":\"add\",\"arguments\":[1.0,-2.5],\"result\":-1.5,\"error\":null}"

    describe "GET /sub/<x>/<y>" $ do
        it "sub - happy case" $ do
            get "/sub/1/2" `shouldRespondWith` "{\"operator\":\"sub\",\"arguments\":[1.0,2.0],\"result\":-1.0,\"error\":null}"
        it "sub - happy case with negative number" $ do
            get "/sub/1/-2.5" `shouldRespondWith` "{\"operator\":\"sub\",\"arguments\":[1.0,-2.5],\"result\":3.5,\"error\":null}"

    describe "GET /mul/<x>/<y>" $ do
        it "mul - happy case" $ do
            get "/mul/1/2" `shouldRespondWith` "{\"operator\":\"mul\",\"arguments\":[1.0,2.0],\"result\":2.0,\"error\":null}"
        it "mul - happy case with one negative number" $ do
            get "/mul/1/-2.5" `shouldRespondWith` "{\"operator\":\"mul\",\"arguments\":[1.0,-2.5],\"result\":-2.5,\"error\":null}"
        it "mul - happy case with two negative numbers" $ do
            get "/mul/-2/-2.5" `shouldRespondWith` "{\"operator\":\"mul\",\"arguments\":[-2.0,-2.5],\"result\":5.0,\"error\":null}"
        it "mul - happy case with multiplication by zero" $ do
            get "/mul/0/2.5" `shouldRespondWith` "{\"operator\":\"mul\",\"arguments\":[0.0,2.5],\"result\":0.0,\"error\":null}"

    describe "GET /div/<x>/<y>" $ do
        it "div - happy case" $ do
            get "/div/16.4/4.1" `shouldRespondWith` "{\"operator\":\"div\",\"arguments\":[16.4,4.1],\"result\":4.0,\"error\":null}"
        it "div - happy case with one negative number" $ do
            get "/div/16.4/-4.1" `shouldRespondWith` "{\"operator\":\"div\",\"arguments\":[16.4,-4.1],\"result\":-4.0,\"error\":null}"
        it "div - happy case with two negative numbers" $ do
            get "/div/-16.4/-4.1" `shouldRespondWith` "{\"operator\":\"div\",\"arguments\":[-16.4,-4.1],\"result\":4.0,\"error\":null}"
        it "div - happy case with multiplication by zero" $ do
            get "/div/0/2.5" `shouldRespondWith` "{\"operator\":\"div\",\"arguments\":[0.0,2.5],\"result\":0.0,\"error\":null}"
        it "div - fails on division by zero" $ do
            get "/div/5.1/0" `shouldRespondWith` "{\"operator\":\"div\",\"arguments\":[5.1,0.0],\"result\":null,\"error\":\"Division by zero\"}"

    describe "GET /sqrt/<x>" $ do
        it "sqrt - happy case" $ do
            get "/sqrt/16.0" `shouldRespondWith` "{\"operator\":\"sqrt\",\"arguments\":[16.0],\"result\":4.0,\"error\":null}"
        it "sqrt - happy case with zero" $ do
            get "/sqrt/0.0" `shouldRespondWith` "{\"operator\":\"sqrt\",\"arguments\":[0.0],\"result\":0.0,\"error\":null}"
        it "sqrt - fails on negative numbers" $ do
            get "/sqrt/-16.0" `shouldRespondWith` "{\"operator\":\"sqrt\",\"arguments\":[-16.0],\"result\":null,\"error\":\"Square root of negative number is a complex number\"}"

    describe "GET /pow/<x>/<n>" $ do
        it "pow - happy case" $ do
            get "/pow/4/2" `shouldRespondWith` "{\"operator\":\"pow\",\"arguments\":[4.0,2.0],\"result\":16.0,\"error\":null}"
        it "pow - happy case with negative base" $ do
            get "/pow/-4.0/3" `shouldRespondWith` "{\"operator\":\"pow\",\"arguments\":[-4.0,3.0],\"result\":-64.0,\"error\":null}"
        it "pow - happy case with negative power" $ do
            get "/pow/0.5/-2" `shouldRespondWith` "{\"operator\":\"pow\",\"arguments\":[0.5,-2.0],\"result\":4.0,\"error\":null}"
        it "pow - happy case with negative base and power" $ do
            get "/pow/-0.5/-2" `shouldRespondWith` "{\"operator\":\"pow\",\"arguments\":[-0.5,-2.0],\"result\":4.0,\"error\":null}"
        it "pow - happy case with zero base" $ do
            get "/pow/0/2" `shouldRespondWith` "{\"operator\":\"pow\",\"arguments\":[0.0,2.0],\"result\":0.0,\"error\":null}"
        it "pow - happy case with zero power" $ do
            get "/pow/2.5/0" `shouldRespondWith` "{\"operator\":\"pow\",\"arguments\":[2.5,0.0],\"result\":1.0,\"error\":null}"
        it "pow - fails on negative power and zero base" $ do
            get "/pow/0.0/-1" `shouldRespondWith` "{\"operator\":\"pow\",\"arguments\":[0.0,-1.0],\"result\":null,\"error\":\"Negative power of zero leads to division by zero\"}"
