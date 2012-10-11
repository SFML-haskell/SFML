module SFML.Graphics.Boundable
where


import SFML.Graphics.Rect


class Boundable a where
    
    -- | Get the local bounding rectangle of a boundable.
    --
    -- The returned rectangle is in local coordinates, which means
    -- that it ignores the transformations (translation, rotation,
    -- scale, ...) that are applied to the entity.
    -- In other words, this function returns the bounds of the
    -- entity in the entity's coordinate system.
    getLocalBounds :: a -> IO FloatRect
    
    -- | Get the global bounding rectangle of a shape.
    --
    -- The returned rectangle is in global coordinates, which means
    -- that it takes in account the transformations (translation,
    -- rotation, scale, ...) that are applied to the entity.
    -- In other words, this function returns the bounds of the
    -- sprite in the global 2D world's coordinate system.
    getGlobalBounds :: a -> IO FloatRect

