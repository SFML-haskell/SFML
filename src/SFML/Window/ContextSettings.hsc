{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.ContextSettings
(
    ContextSettings(..)
)
where


import Foreign.C.Types
import Foreign.Storable


#include <SFML/Window/Window.h>


sizeInt = #{size int}


data ContextSettings = ContextSettings
    { depthBits         :: Int -- ^ Bits of the depth buffer
    , stencilBits       :: Int -- ^ Bits of the stencil buffer
    , antialiasingLevel :: Int -- ^ Level of antialiasing
    , majorVersion      :: Int -- ^ Major number of the context version to create
    , minorVersion      :: Int -- ^ Minor number of the context version to create
    }
    deriving (Show)


instance Storable ContextSettings where
    sizeOf _ = 5*sizeInt
    alignment _ = alignment (undefined :: CUInt)
    
    peek ptr = do
        db <- #{peek sfContextSettings, depthBits} ptr :: IO CInt
        sb <- #{peek sfContextSettings, stencilBits} ptr :: IO CInt
        al <- #{peek sfContextSettings, antialiasingLevel} ptr :: IO CInt
        ma <- #{peek sfContextSettings, majorVersion} ptr :: IO CInt
        mi <- #{peek sfContextSettings, minorVersion} ptr :: IO CInt
        return $ ContextSettings (fromIntegral db) (fromIntegral sb) (fromIntegral al)
            (fromIntegral ma) (fromIntegral mi)
    
    poke ptr (ContextSettings db sb al ma mi) = do
        #{poke sfContextSettings, depthBits} ptr (fromIntegral db :: CInt)
        #{poke sfContextSettings, stencilBits} ptr (fromIntegral sb :: CInt)
        #{poke sfContextSettings, antialiasingLevel} ptr (fromIntegral al :: CInt)
        #{poke sfContextSettings, majorVersion} ptr (fromIntegral ma :: CInt)
        #{poke sfContextSettings, minorVersion} ptr (fromIntegral mi :: CInt)

