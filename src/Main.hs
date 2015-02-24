{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
-- import Data.Binary (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as LB
import Foreign hiding (void)
import Network.Wai.Middleware.RequestLogger
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Web.Scotty

import Audio

-- | Generate a random value of type 'a' with the given seed.
generateWith :: Int -> Gen a -> a
generateWith s g = unGen g (mkQCGen s) 42

-- | Randomly corrupt a given file based on a seed.
corrupt :: Int -> FilePath -> IO B.ByteString
corrupt s f = do
  let gen = generateWith s
  body@(B.PS fp o l) <- B.readFile f
  let count = gen arbitrary
  withForeignPtr fp $ \p -> void $ replicateM count $ do
    let idx = gen $ choose (0, l-1)
    let rep = gen arbitrary :: Word8
    pokeByteOff p (o+idx) rep
  return body

main :: IO ()
main = scotty 9000 $ do
  middleware logStdoutDev

  get "/mpeg" $ do
    seed <- param "seed"
    let mpeg = encode (generateWith seed arbitrary :: MpgFrame)
    addHeader "Content-Type" "audio/mpeg"
    raw mpeg

  get "/corrupt-mpeg" $ do
    seed <- param "seed"
    mpeg <- liftIO $ corrupt seed "dat/test.mp3"
    addHeader "Content-Type" "audio/mpeg"
    raw $ LB.fromStrict mpeg

  get "/corrupt-ogg" $ do
    seed <- param "seed"
    mpeg <- liftIO $ corrupt seed "dat/test.ogg"
    addHeader "Content-Type" "audio/ogg"
    raw $ LB.fromStrict mpeg

  get "/corrupt-wav" $ do
    seed <- param "seed"
    mpeg <- liftIO $ corrupt seed "dat/test.wav"
    addHeader "Content-Type" "audio/wav"
    raw $ LB.fromStrict mpeg

  get "/corrupt-wma" $ do
    seed <- param "seed"
    mpeg <- liftIO $ corrupt seed "dat/test.wma"
    addHeader "Content-Type" "audio/x-ms-wma"
    raw $ LB.fromStrict mpeg
