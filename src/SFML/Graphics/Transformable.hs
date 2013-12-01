module SFML.Graphics.Transformable
where


import SFML.Graphics.Transform
import SFML.System.Vector2


class Transformable a where
    
    -- | Set the position of a transformable.
    --
    -- This function completely overwrites the previous position.
    --
    -- See 'move' to apply an offset based on the previous position instead.
    --
    -- The default position of a transformable object is (0, 0).
    setPosition :: a -> Vec2f -> IO ()
    
    -- | Set the orientation of a transformable.
    --
    -- This function completely overwrites the previous rotation.
    --
    -- See 'rotate' to add an angle based on the previous rotation instead.
    --
    -- The default rotation of a transformable Transformable object is 0.
    setRotation
        :: a
        -> Float -- ^ New rotation, in degrees
        -> IO ()
    
    -- | Set the scale factors of a transformable.
    --
    -- This function completely overwrites the previous scale.
    --
    -- See 'scale' to add a factor based on the previous scale instead.
    --
    -- The default scale of a transformable Transformable object is (1, 1).
    setScale :: a -> Vec2f -> IO ()
    
    -- | Set the local origin of a transformable.
    --
    -- The origin of an object defines the center point for
    -- all transformations (position, scale, rotation).
    --
    -- The coordinates of this point must be relative to the
    -- top-left corner of the object, and ignore all
    -- transformations (position, scale, rotation).
    --
    -- The default origin of a transformable Transformable object is (0, 0).
    setOrigin :: a -> Vec2f -> IO ()
    
    -- | Get the position of a transformable.
    getPosition :: a -> IO Vec2f
    
    -- | Get the orientation of a transformable.
    getRotation
        :: a
        -> IO Float -- ^ Current rotation, in degrees
    
    -- | Get the current scale of a transformable
    getScale :: a -> IO Vec2f
    
    -- | Get the local origin of a transformable.
    getOrigin :: a -> IO Vec2f
    
    -- | Move a transformable by a given offset
    --
    -- This function adds to the current position of the object,
    -- unlike 'setPosition' which overwrites it.
    move :: a -> Vec2f -> IO ()
    
    -- | Rotate a transformable.
    --
    -- This function adds to the current rotation of the object,
    -- unlike 'setRotation' which overwrites it.
    rotate
        :: a
        -> Float -- ^ Angle of rotation, in degrees
        -> IO ()
    
    -- | Scale a transformable.
    --
    -- This function multiplies the current scale of the object,
    -- unlike 'setScale' which overwrites it.
    scale :: a -> Vec2f -> IO ()
    
    -- | Get the combined transform of a transformable.
    getTransform :: a -> IO Transform
    
    -- | Get the inverse of the combined transform of a transformable.
    getInverseTransform :: a -> IO Transform

