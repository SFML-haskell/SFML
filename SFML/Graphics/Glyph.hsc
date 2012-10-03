{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Graphics.Glyph
(
    SFGlyph(..)
)
where


import SFML.Graphics.Rect

import Foreign.Storable

#include <SFML/Graphics/Glyph.h>


sizeInt = #{size int}
sizeIntRect = #{size sfIntRect}


-- | Describes a glyph (a visual character).
data SFGlyph = SFGlyph
    { advance     :: Int       -- ^ Offset to move horizontically to the next character
    , bounds      :: SFIntRect -- ^ Bounding rectangle of the glyph, in coordinates relative to the baseline
    , textureRect :: SFIntRect -- ^ Texture coordinates of the glyph inside the font's image
    }


instance Storable SFGlyph where
    sizeOf _ = sizeInt + 2*sizeIntRect
    alignment _ = alignment (undefined :: SFIntRect)
    
    peek ptr = do
        advance <- #{peek sfGlyph, advance} ptr
        bounds  <- #{peek sfGlyph, bounds} ptr
        rect    <- #{peek sfGlyph, textureRect} ptr
        return $ SFGlyph advance bounds rect
    
    poke ptr (SFGlyph advance bounds rect) = do
        #{poke sfGlyph, advance} ptr advance
        #{poke sfGlyph, bounds} ptr bounds
        #{poke sfGlyph, textureRect} ptr rect

