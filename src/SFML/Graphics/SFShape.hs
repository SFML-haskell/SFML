module SFML.Graphics.SFShape
where


import SFML.Graphics.Color
import SFML.System.Vector2


class SFShape a where
    
    -- | Set the fill color of a shape.
    --
    -- This color is modulated (multiplied) with the shape's
    -- texture if any. It can be used to colorize the shape,
    -- or change its global opacity.
    --
    -- You can use 'Transparent' to make the inside of
    -- the shape transparent, and have the outline alone.
    --
    -- By default, the shape's fill color is opaque white.
    setFillColor :: a -> Color -> IO ()
    
    -- | Set the outline color of a shape.
    --
    -- You can use 'Transparent' to disable the outline.
    --
    -- By default, the shape's outline color is opaque white.
    setOutlineColor :: a -> Color -> IO ()
    
    -- | Set the thickness of a shape's outline.
    --
    -- This number cannot be negative. Using zero disables
    -- the outline.
    --
    -- By default, the outline thickness is 0.
    setOutlineThickness
        :: a
        -> Float -- ^ New outline thickness
        -> IO ()
    
    -- | Get the fill color of a shape.
    getFillColor :: a -> IO Color
    
    -- | Get the outline color of a shape.
    getOutlineColor :: a -> IO Color
    
    -- | Get the outline thickness of a shape.
    getOutlineThickness :: a -> IO Float
    
    -- | Get the total number of points of a shape.
    getPointCount :: a -> IO Int

    -- | Get the ith point of a shape.
    --
    -- The result is undefined if index is out of the valid range.
    getPoint
        :: a
        -> Int -- ^ Index of the point to get, in range [0 .. 'getPointCount' - 1]
        -> IO Vec2f

