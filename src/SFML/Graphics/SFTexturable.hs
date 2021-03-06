module SFML.Graphics.SFTexturable
where


import SFML.Graphics.Color
import SFML.Graphics.Rect
import SFML.Graphics.Types


class SFTexturable a where
    
    -- | Change the source texture of a Texturable.
    --
    -- The texture argument refers to a texture that must
    -- exist as long as the texturable uses it. Indeed, the texturable
    -- doesn't store its own copy of the texture, but rather keeps
    -- a pointer to the one that you passed to this function.
    --
    -- If the source texture is destroyed and the texturable tries to
    -- use it, the behaviour is undefined.
    --
    -- If resetRect is 'True', the TextureRect property of
    -- the texturable is automatically adjusted to the size of the new
    -- texture. If it is false, the texture rect is left unchanged.
    setTexture
        :: a
        -> Texture -- ^ New texture
        -> Bool    -- ^ Should the texture rect be reset to the size of the new texture?
        -> IO ()

    -- | Set the sub-rectangle of the texture that a texturable will display.
    --
    -- The texture rect is useful when you don't want to display
    -- the whole texture, but rather a part of it.
    --
    -- By default, the texture rect covers the entire texture.
    setTextureRect
        :: a
        -> IntRect -- ^ Rectangle defining the region of the texture to display
        -> IO ()
    
    -- | Get the source texture of a texturable.
    --
    -- If the texturable has no source texture, 'Nothing' is returned.
    --
    -- The returned pointer is const, which means that you can't
    -- modify the texture when you retrieve it with this function.
    getTexture :: a -> IO (Maybe Texture)
    
    -- | Get the sub-rectangle of the texture displayed by a texturable.
    getTextureRect :: a -> IO IntRect

