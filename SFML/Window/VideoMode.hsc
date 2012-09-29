{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.VideoMode
(
    SFVideoMode(..)
,   getDesktopMode
,   getFullscreenModes
,   isValid
)
where


import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Storable

#include <SFML/Window/VideoMode.h>


sizeInt = #{size int}


data SFVideoMode = SFVideoMode
    { windowWidth  :: Int
    , windowHeight :: Int
    , windowBPP    :: Int
    }
    deriving (Show)


instance Storable SFVideoMode where
    sizeOf _ = 3*sizeInt
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = do
        w <- #{peek sfVideoMode, width} ptr
        h <- #{peek sfVideoMode, height} ptr
        b <- #{peek sfVideoMode, bitsPerPixel} ptr
        return $ SFVideoMode w h b
    
    poke ptr (SFVideoMode w h b) = do
        #{poke sfVideoMode, width} ptr w
        #{poke sfVideoMode, height} ptr h
        #{poke sfVideoMode, bitsPerPixel} ptr b


-- | Get the current desktop video mode
getDesktopMode = alloca $ \ptr -> sfVideoMode_getDesktopMode_helper ptr >> peek ptr

foreign import ccall "sfVideoMode_getDesktopMode_helper"
    sfVideoMode_getDesktopMode_helper :: Ptr SFVideoMode -> IO ()

--CSFML_WINDOW_API sfVideoMode sfVideoMode_getDesktopMode(void);


-- | Retrieve all the video modes supported in fullscreen mode
--
-- When creating a fullscreen window, the video mode is restricted
-- to be compatible with what the graphics driver and monitor
-- support.

-- This function returns the complete list of all video
-- modes that can be used in fullscreen mode.
--
-- The returned array is sorted from best to worst, so that
-- the first element will always give the best mode (higher
-- width, height and bits-per-pixel).
getFullscreenModes :: IO [SFVideoMode]
getFullscreenModes = do
    alloca $ \countPtr -> do
    ptrVM <- sfVideoMode_getFullscreenModes countPtr
    count <- peek countPtr
    peekArray (fromIntegral count) ptrVM

foreign import ccall "sfVideoMode_getFullscreenModes"
    sfVideoMode_getFullscreenModes :: Ptr CUInt -> IO (Ptr SFVideoMode)

--CSFML_WINDOW_API const sfVideoMode* sfVideoMode_getFullscreenModes(size_t* Count);


-- | Tell whether or not a video mode is valid
--
-- The validity of video modes is only relevant when using
-- fullscreen windows; otherwise any video mode can be used
-- with no restriction.
isValid :: SFVideoMode -> IO Bool
isValid vm = with vm $ \ptrVm -> sfVideoMode_isValid_helper ptrVm >>= return . (/=0)

foreign import ccall "sfVideoMode_isValid_helper"
    sfVideoMode_isValid_helper :: Ptr SFVideoMode -> IO CChar

--CSFML_WINDOW_API sfBool sfVideoMode_isValid(sfVideoMode mode);

