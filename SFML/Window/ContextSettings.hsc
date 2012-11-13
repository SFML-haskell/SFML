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
        db <- #{peek sfContextSettings, depthBits} ptr
        sb <- #{peek sfContextSettings, stencilBits} ptr
        al <- #{peek sfContextSettings, antialiasingLevel} ptr
        ma <- #{peek sfContextSettings, majorVersion} ptr
        mi <- #{peek sfContextSettings, minorVersion} ptr
        return $ ContextSettings db sb al ma mi
    
    poke ptr (ContextSettings db sb al ma mi) = do
        #{poke sfContextSettings, depthBits} ptr db
        #{poke sfContextSettings, stencilBits} ptr sb
        #{poke sfContextSettings, antialiasingLevel} ptr al
        #{poke sfContextSettings, majorVersion} ptr ma
        #{poke sfContextSettings, minorVersion} ptr mi

