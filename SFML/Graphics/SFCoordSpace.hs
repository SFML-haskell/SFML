module SFML.Graphics.SFCoordSpace
where


import SFML.Graphics.Types
import SFML.System.Vector2


class SFCoordSpace a where
    
    -- | Convert a point into view coordinates.
    convertCoords
        :: a
        -> Vec2i -- ^ Point to convert, relative to the object
        -> Maybe View -- ^ Target view to convert the point to ('Nothing' to use the current view)
        -> IO Vec2f

