module SFML.Graphics.ConvexShape
(
    createConvexShape
,   copy
,   destroy
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
,   setPoint
,   getLocalBounds
,   getGlobalBounds
)
where


import SFML.Graphics.SFBoundable
import SFML.Graphics.Color
import SFML.Graphics.Rect
import SFML.Graphics.SFShape
import SFML.Graphics.SFShapeResizable
import SFML.Graphics.SFTexturable
import SFML.Graphics.Transform
import SFML.Graphics.Transformable
import SFML.Graphics.Types
import SFML.SFCopyable
import SFML.SFResource
import SFML.System.Vector2

import Control.Monad ((>=>))
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)


checkNullTexture :: Texture -> Maybe Texture
checkNullTexture tex@(Texture ptr) = if ptr == nullPtr then Nothing else Just tex


-- | Create a new convex shape.
createConvexShape :: IO ConvexShape
createConvexShape = sfConvexShape_create

foreign import ccall unsafe "sfConvexShape_create"
    sfConvexShape_create :: IO ConvexShape

--CSFML_GRAPHICS_API sfConvexShape* sfConvexShape_create(void);


instance SFCopyable ConvexShape where
    
    {-# INLINABLE copy #-}
    copy = sfConvexShape_copy


foreign import ccall unsafe "sfConvexShape_copy"
    sfConvexShape_copy :: ConvexShape -> IO ConvexShape

--CSFML_GRAPHICS_API sfConvexShape* sfConvexShape_copy(sfConvexShape* shape);


instance SFResource ConvexShape where
    
    {-# INLINABLE destroy #-}
    destroy = sfConvexShape_destroy

foreign import ccall unsafe "sfConvexShape_destroy"
    sfConvexShape_destroy :: ConvexShape -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_destroy(sfConvexShape* shape);


instance Transformable ConvexShape where
    
    {-# INLINABLE setPosition #-}
    setPosition c p = with p $ sfConvexShape_setPosition_helper c
    
    {-# INLINABLE setRotation #-}
    setRotation c r = sfConvexShape_setRotation c (realToFrac r)
    
    {-# INLINABLE setScale #-}
    setScale c s = with s $ sfConvexShape_setScale_helper c
    
    {-# INLINABLE setOrigin #-}
    setOrigin c o = with o $ sfConvexShape_setOrigin_helper c
    
    {-# INLINABLE getPosition #-}
    getPosition c = alloca $ \ptr -> sfConvexShape_getPosition_helper c ptr >> peek ptr
    
    {-# INLINABLE getRotation #-}
    getRotation = sfConvexShape_getRotation >=> return . realToFrac
    
    {-# INLINABLE getScale #-}
    getScale c = alloca $ \ptr -> sfConvexShape_getScale_helper c ptr >> peek ptr
    
    {-# INLINABLE getOrigin #-}
    getOrigin c = alloca $ \ptr -> sfConvexShape_getOrigin_helper c ptr >> peek ptr
    
    {-# INLINABLE move #-}
    move c off = with off $ sfConvexShape_move_helper c
    
    {-# INLINABLE rotate #-}
    rotate c a = sfConvexShape_rotate c (realToFrac a)
    
    {-# INLINABLE scale #-}
    scale c s = with s $ sfConvexShape_scale_helper c
    
    {-# INLINABLE getTransform #-}
    getTransform c = alloca $ \ptr -> sfConvexShape_getTransform_helper c ptr >> peek ptr
    
    {-# INLINABLE getInverseTransform #-}
    getInverseTransform c = alloca $ \ptr -> sfConvexShape_getInverseTransform_helper c ptr >> peek ptr


foreign import ccall unsafe "sfConvexShape_setPosition_helper"
    sfConvexShape_setPosition_helper :: ConvexShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setPosition(sfConvexShape* shape, sfVector2f position);

foreign import ccall unsafe "sfConvexShape_setRotation"
    sfConvexShape_setRotation :: ConvexShape -> CFloat -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setRotation(sfConvexShape* shape, float angle);

foreign import ccall unsafe "sfConvexShape_setScale_helper"
    sfConvexShape_setScale_helper :: ConvexShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setScale(sfConvexShape* shape, sfVector2f scale);

foreign import ccall unsafe "sfConvexShape_setOrigin_helper"
    sfConvexShape_setOrigin_helper :: ConvexShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setOrigin(sfConvexShape* shape, sfVector2f origin);

foreign import ccall unsafe "sfConvexShape_getPosition_helper"
    sfConvexShape_getPosition_helper :: ConvexShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfConvexShape_getPosition(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getRotation"
    sfConvexShape_getRotation :: ConvexShape -> IO CFloat

--CSFML_GRAPHICS_API float sfConvexShape_getRotation(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getScale_helper"
    sfConvexShape_getScale_helper :: ConvexShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfConvexShape_getScale(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getOrigin_helper"
    sfConvexShape_getOrigin_helper :: ConvexShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfConvexShape_getOrigin(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_move_helper"
    sfConvexShape_move_helper :: ConvexShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_move(sfConvexShape* shape, sfVector2f offset);

foreign import ccall unsafe "sfConvexShape_rotate"
    sfConvexShape_rotate :: ConvexShape -> CFloat -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_rotate(sfConvexShape* shape, float angle);

foreign import ccall unsafe "sfConvexShape_scale_helper"
    sfConvexShape_scale_helper :: ConvexShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_scale(sfConvexShape* shape, sfVector2f factors);

foreign import ccall unsafe "sfConvexShape_getTransform_helper"
    sfConvexShape_getTransform_helper :: ConvexShape -> Ptr Transform -> IO ()

--CSFML_GRAPHICS_API sfTransform sfConvexShape_getTransform(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getInverseTransform_helper"
    sfConvexShape_getInverseTransform_helper :: ConvexShape -> Ptr Transform -> IO ()

--CSFML_GRAPHICS_API sfTransform sfConvexShape_getInverseTransform(const sfConvexShape* shape);


instance SFTexturable ConvexShape where
    
    {-# INLINABLE setTexture #-}
    setTexture c tex reset = sfConvexShape_setTexture c tex (fromIntegral . fromEnum $ reset)
    
    {-# INLINABLE setTextureRect #-}
    setTextureRect c rect = with rect $ sfConvexShape_setTextureRect_helper c
    
    {-# INLINABLE getTexture #-}
    getTexture = sfConvexShape_getTexture >=> return . checkNullTexture
    
    {-# INLINABLE getTextureRect #-}
    getTextureRect c = alloca $ \ptr -> sfConvexShape_getTextureRect_helper c ptr >> peek ptr


foreign import ccall unsafe "sfConvexShape_setTexture"
    sfConvexShape_setTexture :: ConvexShape -> Texture -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setTexture(sfConvexShape* shape, const sfTexture* texture, sfBool resetRect);

foreign import ccall unsafe "sfConvexShape_setTextureRect_helper"
    sfConvexShape_setTextureRect_helper :: ConvexShape -> Ptr IntRect -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setTextureRect(sfConvexShape* shape, sfIntRect rect);

foreign import ccall unsafe "sfConvexShape_getTexture"
    sfConvexShape_getTexture :: ConvexShape -> IO Texture

--CSFML_GRAPHICS_API const sfTexture* sfConvexShape_getTexture(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getTextureRect_helper"
    sfConvexShape_getTextureRect_helper :: ConvexShape -> Ptr IntRect -> IO ()

--CSFML_GRAPHICS_API sfIntRect sfConvexShape_getTextureRect(const sfConvexShape* shape);


instance SFShape ConvexShape where
    
    {-# INLINABLE setFillColor #-}
    setFillColor c color = with color $ sfConvexShape_setFillColor_helper c
    
    {-# INLINABLE setOutlineColor #-}
    setOutlineColor c color = with color $ sfConvexShape_setOutlineColor_helper c
    
    {-# INLINABLE setOutlineThickness #-}
    setOutlineThickness c t = sfConvexShape_setOutlineThickness c (realToFrac t)
    
    {-# INLINABLE getFillColor #-}
    getFillColor c = alloca $ \ptr -> sfConvexShape_getFillColor_helper c ptr >> peek ptr
    
    {-# INLINABLE getOutlineColor #-}
    getOutlineColor c = alloca $ \ptr -> sfConvexShape_getOutlineColor_helper c ptr >> peek ptr
    
    {-# INLINABLE getOutlineThickness #-}
    getOutlineThickness = sfConvexShape_getOutlineThickness >=> return . realToFrac
    
    {-# INLINABLE getPointCount #-}
    getPointCount = sfConvexShape_getPointCount >=> return . fromIntegral
    
    {-# INLINABLE getPoint #-}
    getPoint c idx = alloca $ \ptr -> sfConvexShape_getPoint_helper c (fromIntegral idx) ptr >> peek ptr


foreign import ccall unsafe "sfConvexShape_setFillColor_helper"
    sfConvexShape_setFillColor_helper :: ConvexShape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setFillColor(sfConvexShape* shape, sfColor color);

foreign import ccall unsafe "sfConvexShape_setOutlineColor_helper"
    sfConvexShape_setOutlineColor_helper :: ConvexShape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setOutlineColor(sfConvexShape* shape, sfColor color);

foreign import ccall unsafe "sfConvexShape_setOutlineThickness"
    sfConvexShape_setOutlineThickness :: ConvexShape -> CFloat -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setOutlineThickness(sfConvexShape* shape, float thickness);

foreign import ccall unsafe "sfConvexShape_getFillColor_helper"
    sfConvexShape_getFillColor_helper :: ConvexShape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API sfColor sfConvexShape_getFillColor(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getOutlineColor_helper"
    sfConvexShape_getOutlineColor_helper :: ConvexShape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API sfColor sfConvexShape_getOutlineColor(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getOutlineThickness"
    sfConvexShape_getOutlineThickness :: ConvexShape -> IO CFloat

--CSFML_GRAPHICS_API float sfConvexShape_getOutlineThickness(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getPointCount"
    sfConvexShape_getPointCount :: ConvexShape -> IO CUInt

--CSFML_GRAPHICS_API unsigned int sfConvexShape_getPointCount(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getPoint_helper"
    sfConvexShape_getPoint_helper :: ConvexShape -> CUInt -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfConvexShape_getPoint(const sfConvexShape* shape, unsigned int index);


instance SFShapeResizable ConvexShape where

    {-# INLINABLE setPointCount #-}
    setPointCount c count = sfConvexShape_setPointCount c (fromIntegral count)


foreign import ccall unsafe "sfConvexShape_setPointCount"
    sfConvexShape_setPointCount :: ConvexShape -> CUInt -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setPointCount(sfConvexShape* shape, unsigned int count);


-- | Set the position of a point in a convex shape.
--
-- Don't forget that the polygon must remain convex, and
-- the points need to stay ordered!
--
-- 'setPointCount' must be called first in order to set the total
-- number of points. The result is undefined if the index is out
-- of the valid range.
setPoint
    :: ConvexShape
    -> Int   -- ^ Index of the point to change, in range [0 .. 'getPointCount' - 1]
    -> Vec2f -- ^ New point
    -> IO ()

setPoint s idx p = with p $ sfConvexShape_setPoint_helper s (fromIntegral idx)

foreign import ccall unsafe "sfConvexShape_setPoint_helper"
    sfConvexShape_setPoint_helper :: ConvexShape -> CUInt -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfConvexShape_setPoint(sfConvexShape* shape, unsigned int index, sfVector2f point);


instance SFBoundable ConvexShape where

    {-# INLINABLE getLocalBounds #-}
    getLocalBounds c = alloca $ \ptr -> sfConvexShape_getLocalBounds_helper c ptr >> peek ptr
    
    {-# INLINABLE getGlobalBounds #-}
    getGlobalBounds c = alloca $ \ptr -> sfConvexShape_getGlobalBounds_helper c ptr >> peek ptr

foreign import ccall unsafe "sfConvexShape_getLocalBounds_helper"
    sfConvexShape_getLocalBounds_helper :: ConvexShape -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfConvexShape_getLocalBounds(const sfConvexShape* shape);

foreign import ccall unsafe "sfConvexShape_getGlobalBounds_helper"
    sfConvexShape_getGlobalBounds_helper :: ConvexShape -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfConvexShape_getGlobalBounds(const sfConvexShape* shape);

