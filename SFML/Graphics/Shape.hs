module SFML.Graphics.Shape
(
    createShape
,   destroyShape
,   setPosition
,   setRotation
,   setScale
,   setOrigin
,   getPosition
,   getRotation
,   getScale
,   getOrigin
,   move
,   rotate
,   scale
,   getTransform
,   getInverseTransform
,   setTexture
,   setTextureRect
,   getTexture
,   getTextureRect
,   setFillColor
,   setOutlineColor
,   setOutlineThickness
,   getFillColor
,   getOutlineColor
,   getOutlineThickness
,   getPointCount
,   getPoint
,   setPointCount
,   getLocalBounds
,   getGlobalBounds
,   updateShape
)
where


import SFML.Graphics.Boundable
import SFML.Graphics.Color
import SFML.Graphics.Rect
import SFML.Graphics.SFShape
import SFML.Graphics.SFShapeResizable
import SFML.Graphics.Texturable
import SFML.Graphics.Transform
import SFML.Graphics.Transformable
import SFML.Graphics.Types
import SFML.System.Vector2

import Control.Monad ((>=>))
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)
import Unsafe.Coerce


checkNullTexture :: Texture -> Maybe Texture
checkNullTexture tex@(Texture ptr) =
    case (unsafeCoerce ptr) of
        0 -> Nothing
        _ -> Just tex


-- | Create a new shape.
createShape
    :: Ptr a -- ^ Callback that provides the point count of the shape
    -> Ptr b -- ^ Callback that provides the points of the shape
    -> Ptr c -- ^ Data to pass to the callback functions
    -> IO Shape

createShape = sfShape_create

foreign import ccall unsafe "sfShape_create"
    sfShape_create :: Ptr a -> Ptr b -> Ptr c -> IO Shape

--CSFML_GRAPHICS_API sfShape* sfShape_create(sfShapeGetPointCountCallback getPointCount, sfShapeGetPointCallback getPoint, void* userData);


-- | Destroy an existing shape.
destroyShape = sfShape_destroy

foreign import ccall unsafe "sfShape_destroy"
    sfShape_destroy :: Shape -> IO ()

--CSFML_GRAPHICS_API void sfShape_destroy(sfShape* shape);


instance Transformable Shape where
    
    setPosition c p = with p $ sfShape_setPosition_helper c
    
    setRotation = sfShape_setRotation
    
    setScale c s = with s $ sfShape_setScale_helper c
    
    setOrigin c o = with o $ sfShape_setOrigin_helper c
    
    getPosition c = alloca $ \ptr -> sfShape_getPosition_helper c ptr >> peek ptr
    
    getRotation = sfShape_getRotation
    
    getScale c = alloca $ \ptr -> sfShape_getScale_helper c ptr >> peek ptr
    
    getOrigin c = alloca $ \ptr -> sfShape_getOrigin_helper c ptr >> peek ptr
    
    move c off = with off $ sfShape_move_helper c
    
    rotate = sfShape_rotate
    
    scale c s = with s $ sfShape_scale_helper c
    
    getTransform c = alloca $ \ptr -> sfShape_getTransform_helper c ptr >> peek ptr
    
    getInverseTransform c = alloca $ \ptr -> sfShape_getInverseTransform_helper c ptr >> peek ptr


foreign import ccall unsafe "sfShape_setPosition_helper"
    sfShape_setPosition_helper :: Shape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfShape_setPosition(sfShape* shape, sfVector2f position);

foreign import ccall unsafe "sfShape_setRotation"
    sfShape_setRotation :: Shape -> Float -> IO ()

--CSFML_GRAPHICS_API void sfShape_setRotation(sfShape* shape, float angle);

foreign import ccall unsafe "sfShape_setScale_helper"
    sfShape_setScale_helper :: Shape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfShape_setScale(sfShape* shape, sfVector2f scale);

foreign import ccall unsafe "sfShape_setOrigin_helper"
    sfShape_setOrigin_helper :: Shape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfShape_setOrigin(sfShape* shape, sfVector2f origin);

foreign import ccall unsafe "sfShape_getPosition_helper"
    sfShape_getPosition_helper :: Shape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfShape_getPosition(const sfShape* shape);

foreign import ccall unsafe "sfShape_getRotation"
    sfShape_getRotation :: Shape -> IO Float

--CSFML_GRAPHICS_API float sfShape_getRotation(const sfShape* shape);

foreign import ccall unsafe "sfShape_getScale_helper"
    sfShape_getScale_helper :: Shape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfShape_getScale(const sfShape* shape);

foreign import ccall unsafe "sfShape_getOrigin_helper"
    sfShape_getOrigin_helper :: Shape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfShape_getOrigin(const sfShape* shape);

foreign import ccall unsafe "sfShape_move_helper"
    sfShape_move_helper :: Shape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfShape_move(sfShape* shape, sfVector2f offset);

foreign import ccall unsafe "sfShape_rotate"
    sfShape_rotate :: Shape -> Float -> IO ()

--CSFML_GRAPHICS_API void sfShape_rotate(sfShape* shape, float angle);

foreign import ccall unsafe "sfShape_scale_helper"
    sfShape_scale_helper :: Shape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfShape_scale(sfShape* shape, sfVector2f factors);

foreign import ccall unsafe "sfShape_getTransform_helper"
    sfShape_getTransform_helper :: Shape -> Ptr Transform -> IO ()

--CSFML_GRAPHICS_API sfTransform sfShape_getTransform(const sfShape* shape);

foreign import ccall unsafe "sfShape_getInverseTransform_helper"
    sfShape_getInverseTransform_helper :: Shape -> Ptr Transform -> IO ()

--CSFML_GRAPHICS_API sfTransform sfShape_getInverseTransform(const sfShape* shape);


instance Texturable Shape where
    
    setTexture c tex reset = sfShape_setTexture c tex (fromIntegral . fromEnum $ reset)
    
    setTextureRect c rect = with rect $ sfShape_setTextureRect_helper c
    
    getTexture = sfShape_getTexture >=> return . checkNullTexture
    
    getTextureRect c = alloca $ \ptr -> sfShape_getTextureRect_helper c ptr >> peek ptr


foreign import ccall unsafe "sfShape_setTexture"
    sfShape_setTexture :: Shape -> Texture -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfShape_setTexture(sfShape* shape, const sfTexture* texture, sfBool resetRect);

foreign import ccall unsafe "sfShape_setTextureRect_helper"
    sfShape_setTextureRect_helper :: Shape -> Ptr IntRect -> IO ()

--CSFML_GRAPHICS_API void sfShape_setTextureRect(sfShape* shape, sfIntRect rect);

foreign import ccall unsafe "sfShape_getTexture"
    sfShape_getTexture :: Shape -> IO Texture

--CSFML_GRAPHICS_API const sfTexture* sfShape_getTexture(const sfShape* shape);

foreign import ccall unsafe "sfShape_getTextureRect_helper"
    sfShape_getTextureRect_helper :: Shape -> Ptr IntRect -> IO ()

--CSFML_GRAPHICS_API sfIntRect sfShape_getTextureRect(const sfShape* shape);


instance SFShape Shape where
    
    setFillColor c color = with color $ sfShape_setFillColor_helper c
    
    setOutlineColor c color = with color $ sfShape_setOutlineColor_helper c
    
    setOutlineThickness = sfShape_setOutlineThickness
    
    getFillColor c = alloca $ \ptr -> sfShape_getFillColor_helper c ptr >> peek ptr
    
    getOutlineColor c = alloca $ \ptr -> sfShape_getOutlineColor_helper c ptr >> peek ptr
    
    getOutlineThickness = sfShape_getOutlineThickness
    
    getPointCount = sfShape_getPointCount >=> return . fromIntegral
    
    getPoint c idx = alloca $ \ptr -> sfShape_getPoint_helper c (fromIntegral idx) ptr >> peek ptr


foreign import ccall unsafe "sfShape_setFillColor_helper"
    sfShape_setFillColor_helper :: Shape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfShape_setFillColor(sfShape* shape, sfColor color);

foreign import ccall unsafe "sfShape_setOutlineColor_helper"
    sfShape_setOutlineColor_helper :: Shape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfShape_setOutlineColor(sfShape* shape, sfColor color);

foreign import ccall unsafe "sfShape_setOutlineThickness"
    sfShape_setOutlineThickness :: Shape -> Float -> IO ()

--CSFML_GRAPHICS_API void sfShape_setOutlineThickness(sfShape* shape, float thickness);

foreign import ccall unsafe "sfShape_getFillColor_helper"
    sfShape_getFillColor_helper :: Shape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API sfColor sfShape_getFillColor(const sfShape* shape);

foreign import ccall unsafe "sfShape_getOutlineColor_helper"
    sfShape_getOutlineColor_helper :: Shape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API sfColor sfShape_getOutlineColor(const sfShape* shape);

foreign import ccall unsafe "sfShape_getOutlineThickness"
    sfShape_getOutlineThickness :: Shape -> IO Float

--CSFML_GRAPHICS_API float sfShape_getOutlineThickness(const sfShape* shape);

foreign import ccall "sfShape_getPointCount"
    sfShape_getPointCount :: Shape -> IO CUInt

--CSFML_GRAPHICS_API unsigned int sfShape_getPointCount(const sfShape* shape);

foreign import ccall "sfShape_getPoint_helper"
    sfShape_getPoint_helper :: Shape -> CUInt -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfShape_getPoint(const sfShape* shape, unsigned int index);


instance Boundable Shape where

    getLocalBounds c = alloca $ \ptr -> sfShape_getLocalBounds_helper c ptr >> peek ptr
    
    getGlobalBounds c = alloca $ \ptr -> sfShape_getGlobalBounds_helper c ptr >> peek ptr

foreign import ccall unsafe "sfShape_getLocalBounds_helper"
    sfShape_getLocalBounds_helper :: Shape -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfShape_getLocalBounds(const sfShape* shape);

foreign import ccall unsafe "sfShape_getGlobalBounds_helper"
    sfShape_getGlobalBounds_helper :: Shape -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfShape_getGlobalBounds(const sfShape* shape);


-- | Recompute the internal geometry of a shape.
--
-- This function must be called by specialized shape objects
-- everytime their points change (ie. the result of either
-- the getPointCount or getPoint callbacks is different).
updateShape :: Shape -> IO ()
updateShape = sfShape_update

foreign import ccall unsafe "sfShape_update"
    sfShape_update :: Shape -> IO ()

--CSFML_GRAPHICS_API void sfShape_update(sfShape* shape);

