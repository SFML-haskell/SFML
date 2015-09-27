{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.ContextSettings
(
    ContextSettings(..)
,   ContextAttribute(..)
)
where


import Control.Applicative ((<$>), (<*>))
import Data.Word (Word32)
import Foreign.C.Types
import Foreign.Storable


#include <SFML/Window/Window.h>


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t(y__); }, y__)


data ContextSettings = ContextSettings
    { depthBits         :: Int -- ^ Bits of the depth buffer
    , stencilBits       :: Int -- ^ Bits of the stencil buffer
    , antialiasingLevel :: Int -- ^ Level of antialiasing
    , majorVersion      :: Int -- ^ Major number of the context version to create
    , minorVersion      :: Int -- ^ Minor number of the context version to create
    , attributeFlags    :: [ContextAttribute] -- ^ The attribute flags to create the context with
    }
    deriving (Show)


instance Storable ContextSettings where
    sizeOf _ = #{size sfContextSettings}
    alignment _ = #{alignment sfContextSettings}

    peek ptr = ContextSettings
            <$> fmap fromIntegral (#{peek sfContextSettings, depthBits} ptr :: IO CInt)
            <*> fmap fromIntegral (#{peek sfContextSettings, stencilBits} ptr :: IO CInt)
            <*> fmap fromIntegral (#{peek sfContextSettings, antialiasingLevel} ptr :: IO CInt)
            <*> fmap fromIntegral (#{peek sfContextSettings, majorVersion} ptr :: IO CInt)
            <*> fmap fromIntegral (#{peek sfContextSettings, minorVersion} ptr :: IO CInt)
            <*> fmap (toFlags . fromIntegral) (#{peek sfContextSettings, attributeFlags} ptr :: IO Word32)

    poke ptr (ContextSettings db sb al ma mi af) = do
        #{poke sfContextSettings, depthBits} ptr (fromIntegral db :: CInt)
        #{poke sfContextSettings, stencilBits} ptr (fromIntegral sb :: CInt)
        #{poke sfContextSettings, antialiasingLevel} ptr (fromIntegral al :: CInt)
        #{poke sfContextSettings, majorVersion} ptr (fromIntegral ma :: CInt)
        #{poke sfContextSettings, minorVersion} ptr (fromIntegral mi :: CInt)
        #{poke sfContextSettings, attributeFlags} ptr ((fromIntegral . fromFlags) af :: Word32)


data ContextAttribute
    = ContextDefault -- ^ Non-debug, compatibility context (this and the core attribute are mutually exclusive)
    | ContextCore    -- ^ Core attribute
    | ContextDebug   -- ^ Debug attribute
    deriving (Eq, Show)


instance Enum ContextAttribute where

    fromEnum ContextDefault = 0
    fromEnum ContextCore    = 1
    fromEnum ContextDebug   = 2

    toEnum 0 = ContextDefault
    toEnum 1 = ContextCore
    toEnum 2 = ContextDebug

fromFlags :: [ContextAttribute] -> Int
fromFlags = sum . map fromEnum

toFlags :: Int -> [ContextAttribute]
toFlags = return . toEnum
