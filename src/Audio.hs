{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Audio where

import Data.Binary
import Data.Binary.Put
import Data.Bits
-- import Data.Binary.Bits
-- import Data.Binary.Bits.Get
-- import Data.Binary.Bits.Put
import Data.ByteString hiding (split)
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as LB
import Data.Word
import Foreign
import System.Random
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

data MpgFrame
  = MpgFrame
    { mpgVersion :: MpgVersion
    , mpgLayer   :: MpgLayer
    , mpgProt    :: Bool
    , mpgBitrate :: MpgBitrate
    , mpgSample  :: MpgSample
    , mpgPadding :: Bool
    , mpgPrivate :: Bool
    , mpgChannel :: MpgChannel
    , mpgModeExt :: MpgModeExt
    , mpgCopy    :: Bool
    , mpgOrig    :: Bool
    , mpgEmph    :: MpgEmph
    , mpgData    :: ByteString
    }

instance Arbitrary MpgFrame where
  arbitrary = do
    mpgVersion <- arbitrary
    mpgLayer   <- arbitrary
    let mpgProt = True
    mpgBitrate <- arbitrary
    mpgSample  <- arbitrary
    mpgPadding <- arbitrary
    mpgPrivate <- arbitrary
    mpgChannel <- arbitrary
    mpgModeExt <- arbitrary
    mpgCopy    <- arbitrary
    mpgOrig    <- arbitrary
    mpgEmph    <- arbitrary
    dataLen    <- choose (250000, 500000)
    -- randomShit <- infiniteListOf arbitrary
    -- let uf (w:ws) = Just (w, ws)
    -- let mpgData = fst $ unfoldrN dataLen uf randomShit
    -- -- mpgData    <- fmap pack (vectorOf dataLen arbitrary)
    qcgen      <- getQCGen
    let mpgData = unsafeCreate dataLen $ \p ->
                    let go !n q | n == dataLen
                                = return ()
                                | otherwise
                                = do let (q1,q2) = split q
                                     pokeByteOff p n (unGen arbitrary q1 42 :: Word8)
                                     go (n+1) q2
                    in go 0 qcgen
    return MpgFrame {..}

getQCGen :: Gen QCGen
getQCGen = MkGen $ \q _ -> q

encode :: MpgFrame -> LB.ByteString
encode MpgFrame {..} = LB.fromChunks [header, mpgData]
  where
    toWord :: Enum a => a -> Word8
    toWord = fromIntegral . fromEnum
    (<<)   = shiftL
    header = unsafeCreate 4 $ \p -> do
               pokeByteOff p 0 (0xff :: Word8)
               pokeByteOff p 1 $ 0 .|. 0xff << 5
                                   .|. toWord mpgVersion << 3
                                   .|. toWord mpgLayer << 1
                                   .|. toWord mpgProt
               pokeByteOff p 2 $ 0 .|. toWord mpgBitrate << 4
                                   .|. toWord mpgSample << 2
                                   .|. toWord mpgPadding << 1
                                   .|. toWord mpgPrivate
               pokeByteOff p 3 $ 0 .|. toWord mpgChannel << 6
                                   .|. toWord mpgModeExt << 4
                                   .|. toWord mpgCopy << 3
                                   .|. toWord mpgOrig << 2
                                   .|. toWord mpgEmph

-- instance Binary MpgFrame where
--   put MpgFrame {..}
--     = do let frameSync = 0xff :: Word8
--              fsProt    = frameSync .&. fromIntegral (fromEnum mpgProt)
--              fsLayer   = fsProt .&. (fromIntegral (fromEnum mpgLayer) `shiftL` 1)
--              fsVers    = fsLayer .&. (fromIntegral (fromEnum mpgVersion) `shiftL` 3)
             
--          putWord8 0xff    -- frame sync pt 1
--          putWord8 fsVers
--     -- = do putWord8 0xff -- frame sync pt 1
--     --      putWord8 $ 0 
--     -- = runBitPut $ do
--     --     putBits 11 (0xffff :: Word16) -- frame sync
--     --     putBits 2  mpgVersion
--     --     putBits 2  mpgLayer
--     --     putBool    mpgProt
--     --     putBits 4  mpgBitrate
--     --     putBits 2  mpgSample
--     --     putBool    mpgPadding
--     --     putBool    mpgPrivate
--     --     putBits 2  mpgChannel
--     --     putBits 2  mpgModeExt
--     --     putBool    mpgCopy
--     --     putBool    mpgOrig
--     --     putBits 2  mpgEmph
--     --     putByteString mpgData
--   get = undefined

data MpgVersion
  = MpgV1
  | MpgVR
  | MpgV2
  | MpgV25
  deriving Enum

instance Arbitrary MpgVersion where
  arbitrary = elements [MpgV1 .. MpgV25]

-- instance BinaryBit MpgVersion where
--   putBits n v = putWord8 n (fromIntegral $ fromEnum v)
--   getBits n = fmap (toEnum . fromIntegral) $ getWord8 n

data MpgLayer
  = MpgLR
  | MpgL1
  | MpgL2
  | MpgL3
  deriving Enum

instance Arbitrary MpgLayer where
  arbitrary = elements [MpgLR .. MpgL3]

-- instance BinaryBit MpgLayer where
--   putBits n v = putWord8 n (fromIntegral $ fromEnum v)
--   getBits n = fmap (toEnum . fromIntegral) $ getWord8 n

newtype MpgBitrate = MpgBitrate Word4
  deriving (Arbitrary, Enum)

newtype MpgSample = MpgSample Word4
  deriving (Arbitrary, Enum)

-- | "4 bit" word
newtype Word4 = Word4 Word8
  deriving Enum

instance Arbitrary Word4 where
  arbitrary = fmap (Word4 . fromInteger) $ choose (0, 15)

-- instance BinaryBit Word4 where
--   putBits n (Word4 w) = putWord8 n w
--   getBits n = fmap Word4 $ getWord8 n

data MpgChannel
  = MpgCS  -- stereo
  | MpgCJS -- joint stereo
  | MpgCDC -- dual channel
  | MpgCSC -- single channel
  deriving Enum

instance Arbitrary MpgChannel where
  arbitrary = elements [MpgCS .. MpgCSC]

-- instance BinaryBit MpgChannel where
--   putBits n v = putWord8 n (fromIntegral $ fromEnum v)
--   getBits n = fmap (toEnum . fromIntegral) $ getWord8 n

-- | "2 bit" word
newtype Word2 = Word2 Word8
  deriving Enum

instance Arbitrary Word2 where
  arbitrary = fmap (Word2 . fromInteger) $ choose (0, 3)

-- instance BinaryBit Word2 where
--   putBits n (Word2 w) = putWord8 n w
--   getBits n = fmap Word2 $ getWord8 n

newtype MpgModeExt = MpgModeExt Word2
  deriving (Arbitrary, Enum)

newtype MpgEmph = MpgEmph Word2
  deriving (Arbitrary, Enum)
