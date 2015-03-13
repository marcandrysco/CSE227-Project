{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Text.Printf
-- import Data.Binary (encode)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Internal             as B
import qualified Data.ByteString.Lazy                 as LB
import           Foreign                              hiding (void)
import           Network.Wai.Middleware.RequestLogger
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random
import           Web.Scotty

import           Audio

-- | Generate a random value of type 'a' with the given seed.
generateWith :: Int -> Gen a -> a
generateWith s g = unGen g (mkQCGen s) 42

genIdxs :: Int -> Int -> [(Int, Word8)]
genIdxs s l =
  generateWith s $ do
    count <- choose (10, 10000)
    replicateM count $ 
      (,) <$> choose (0, l-1) 
          <*> (arbitrary :: Gen Word8)

-- | Randomly corrupt a given file based on a seed.
corrupt :: Int -> FilePath -> IO B.ByteString
corrupt s f = do
  body@(B.PS fp o l) <- B.readFile f
  let idxs = genIdxs s l
  withForeignPtr fp $ \p -> forM_ idxs $ \(idx, rep) ->
    pokeByteOff p (o+idx) rep
  return body

dumpCorruption :: Int -> FilePath -> IO ()
dumpCorruption s f = do
  B.PS _ _ l <- B.readFile f
  let idxs = map (\(i,r) -> printf "%d,%d" i r) $ sort $ genIdxs s l
  writeFile (printf "%s-%d" f s) (unlines idxs)

main :: IO ()
main = scotty 9000 $ do
  middleware logStdoutDev

  get "/mpeg" $ do
    seed <- param "seed"
    let mpeg = encode (generateWith seed arbitrary :: MpgFrame)
    addHeader "Content-Type" "audio/mpeg"
    raw mpeg

  corruptFile "/corrupt-mpeg" "dat/test.mp3"   "audio/mpeg"
  corruptFile "/corrupt-ogg"  "dat/test.ogg"   "audio/ogg"
  corruptFile "/corrupt-wav"  "dat/test.wav"   "audio/wav"
  corruptFile "/corrupt-wma"  "dat/test.wma"   "audio/x-ms-wma"
  
  -- seeds for bad mp4s: 22 32 60 75 100
  corruptFile "/corrupt-mp4"  "dat/small.mp4"  "video/mp4"
  corruptFile "/corrupt-ogv"  "dat/small.ogv"  "video/ogg"
  corruptFile "/corrupt-webm" "dat/small.webm" "video/webm"
  corruptFile "/corrupt-3gp"  "dat/small.3gp"  "video/3gp"
  corruptFile "/corrupt-flv"  "dat/small.flv"  "video/x-flv"

corruptFile path file ctype = get path $ do
  seed <- param "seed"
  body <- liftIO $ corrupt seed file
  addHeader "Content-Type" ctype
  raw $ LB.fromStrict body
