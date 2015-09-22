module SFML.Graphics.Color
(
    Color(..)
,   black
,   white
,   red
,   green
,   blue
,   yellow
,   magenta
,   cyan
,   transparent
)
where


import Data.Bits ((.&.), shiftR)
import Data.Word (Word8, Word32)
import Foreign.Storable

#include <SFML/Graphics/Color.h>


-- | Utility data type for manpulating RGBA colors.
data Color = Color
    { r :: Word8
    , g :: Word8
    , b :: Word8
    , a :: Word8
    }
    deriving (Eq, Show)


instance Storable Color where
    sizeOf _ = 4 * sizeOf (undefined :: Word8)
    alignment _ = alignment (undefined :: Word8)

    peek ptr = do
        r <- #{peek sfColor, r} ptr
        g <- #{peek sfColor, g} ptr
        b <- #{peek sfColor, b} ptr
        a <- #{peek sfColor, a} ptr
        return $ Color r g b a

    poke ptr (Color r g b a) = do
        #{poke sfColor, r} ptr r
        #{poke sfColor, g} ptr g
        #{poke sfColor, b} ptr b
        #{poke sfColor, a} ptr a


black       = Color   0   0   0 255
white       = Color 255 255 255 255
red         = Color 255   0   0 255
green       = Color   0 255   0 255
blue        = Color   0   0 255 255
yellow      = Color 255 255   0 255
magenta     = Color 255   0 255 255
cyan        = Color   0 255 255 255
transparent = Color   0   0   0 0


instance Num Color where

    (Color r1 g1 b1 a1) + (Color r2 g2 b2 a2) = Color (r1+r2) (g1+g2) (b1+b2) (a1+a2)
    (Color r1 g1 b1 a1) - (Color r2 g2 b2 a2) = Color (r1-r2) (g1-g2) (b1-b2) (a1-a2)
    (Color r1 g1 b1 a1) * (Color r2 g2 b2 a2) = Color (r1*r2) (g1*g2) (b1*b2) (a1*a2)
    negate (Color r g b a) = Color (-r) (-g) (-b) (-a)
    abs (Color r g b a) = Color (abs r) (abs g) (abs b) (abs a)
    signum (Color r g b a) = Color (signum r) (signum g) (signum b) (signum a)
    fromInteger i' =
        let i = fromIntegral i' :: Word32
            r = fromIntegral $ (i .&. 0xFF000000) `shiftR` 24
            g = fromIntegral $ (i .&. 0x00FF0000) `shiftR` 16
            b = fromIntegral $ (i .&. 0x0000FF00) `shiftR` 8
            a = fromIntegral $  i .&. 0x000000FF
        in Color r g b a
