{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Graphics.Glyph
(
    Glyph(..)
)
where


import SFML.Graphics.Rect

import Foreign.C.Types
import Foreign.Storable

#include <SFML/Graphics/Glyph.h>


sizeInt = #{size int}
sizeIntRect = #{size sfIntRect}


-- | Describes a glyph (a visual character).
data Glyph = Glyph
    { advance     :: Int     -- ^ Offset to move horizontically to the next character
    , bounds      :: IntRect -- ^ Bounding rectangle of the glyph, in coordinates relative to the baseline
    , textureRect :: IntRect -- ^ Texture coordinates of the glyph inside the font's image
    }


instance Storable Glyph where
    sizeOf _ = sizeInt + 2*sizeIntRect
    alignment _ = alignment (undefined :: IntRect)
    
    peek ptr = do
        advance <- #{peek sfGlyph, advance} ptr :: IO CInt
        bounds  <- #{peek sfGlyph, bounds} ptr
        rect    <- #{peek sfGlyph, textureRect} ptr
        return $ Glyph (fromIntegral advance) bounds rect
    
    poke ptr (Glyph advance bounds rect) = do
        #{poke sfGlyph, advance} ptr (fromIntegral advance :: CInt)
        #{poke sfGlyph, bounds} ptr bounds
        #{poke sfGlyph, textureRect} ptr rect

