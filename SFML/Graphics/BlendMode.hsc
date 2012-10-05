{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Graphics.BlendMode
(
    BlendMode(..)
)
where


import Foreign.C.Types (CInt)
import Foreign.Ptr
import Foreign.Storable

#include <SFML/Graphics/BlendMode.h>


-- | Available blending modes for drawing.
data BlendMode
    = BlendAlpha     -- ^ Pixel = Src * a + Dest * (1 - a)
    | BlendAdd       -- ^ Pixel = Src + Dest
    | BlendMultiply  -- ^ Pixel = Src * Dest
    | BlendNone      -- ^ No blending
    deriving (Eq, Enum, Bounded, Show)


instance Storable BlendMode where
    sizeOf _ = sizeInt
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = fmap (toEnum . fromIntegral) $ peek (castPtr ptr :: Ptr CInt)
    poke ptr bm = poke (castPtr ptr :: Ptr CInt) (fromIntegral . fromEnum $ bm)


sizeInt = #{size int}

{-typedef enum 
{
    sfBlendAlpha,    ///< Pixel = Src * a + Dest * (1 - a)
    sfBlendAdd,      ///< Pixel = Src + Dest
    sfBlendMultiply, ///< Pixel = Src * Dest
    sfBlendNone      ///< No blending
} sfBlendMode;-}

