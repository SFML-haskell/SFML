module SFML.Graphics.SFCoordSpace
where


import SFML.Graphics.Types
import SFML.System.Vector2


class SFCoordSpace a where
    
    -- | Convert a point to world coordinates
    --
    -- This function finds the 2D position that matches the
    -- given pixel of the coord space. In other words, it does
    -- the inverse of what the graphics card does, to find the
    -- initial position of a rendered pixel.
    --
    -- Initially, both coordinate systems (world units and target pixels)
    -- match perfectly. But if you define a custom view or resize your
    -- coord space, this assertion is not true anymore, ie. a point
    -- located at (10, 50) in your coord space may map to the point
    -- (150, 75) in your 2D world -- if the view is translated by (140, 25).
    --
    -- This version uses a custom view for calculations, see the other
    -- overload of the function if you want to use the current view of the
    -- render-texture.
    mapPixelToCoords
        :: a
        -> Vec2i -- ^ Pixel to convert
        -> Maybe View -- ^ The view to use for converting the point
        -> IO Vec2f

