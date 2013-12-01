module SFML.Graphics.SFShapeResizable
where


class SFShapeResizable a where
    
    -- | Set the number of points of a resizable shape.
    setPointCount
        :: a
        -> Int -- ^ New number of points of the shape
        -> IO ()

