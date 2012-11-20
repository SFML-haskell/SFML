{-# LANGUAGE DeriveDataTypeable #-}
module SFML.Graphics.RectangleShape
(
    module SFML.Utils
,   RectangleShapeException(..)
,   createRectangleShape
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
,   setSize
,   getSize
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
import SFML.Utils

import Control.Exception
import Control.Monad ((>=>))
import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)


checkNull :: RectangleShape -> Maybe RectangleShape
checkNull rs@(RectangleShape ptr) = if ptr == nullPtr then Nothing else Just rs


checkNullTexture :: Texture -> Maybe Texture
checkNullTexture tex@(Texture ptr) = if ptr == nullPtr then Nothing else Just tex


data RectangleShapeException = RectangleShapeException String deriving (Show, Typeable)

instance Exception RectangleShapeException


-- | Create a new rectangle shape.
createRectangleShape :: IO (Either RectangleShapeException RectangleShape)
createRectangleShape =
    let err = RectangleShapeException "Failed creating rectangle shape"
    in fmap (tagErr err . checkNull) sfRectangleShape_create

foreign import ccall unsafe "sfRectangleShape_create"
    sfRectangleShape_create :: IO RectangleShape

-- \return A new sfRectangleShape object, or NULL if it failed

--CSFML_GRAPHICS_API sfRectangleShape* sfRectangleShape_create(void);


instance SFCopyable RectangleShape where
    
    {-# INLINABLE copy #-}
    copy = sfRectangleShape_copy


foreign import ccall unsafe "sfRectangleShape_copy"
    sfRectangleShape_copy :: RectangleShape -> IO RectangleShape

--CSFML_GRAPHICS_API sfRectangleShape* sfRectangleShape_copy(sfRectangleShape* shape);


instance SFResource RectangleShape where
    
    {-# INLINABLE destroy #-}
    destroy = sfRectangleShape_destroy

foreign import ccall unsafe "sfRectangleShape_destroy"
    sfRectangleShape_destroy :: RectangleShape -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_destroy(sfRectangleShape* shape);

instance Transformable RectangleShape where
    
    {-# INLINABLE setPosition #-}
    setPosition c p = with p $ sfRectangleShape_setPosition_helper c
    
    {-# INLINABLE setRotation #-}
    setRotation s r = sfRectangleShape_setRotation s (realToFrac r)
    
    {-# INLINABLE setScale #-}
    setScale c s = with s $ sfRectangleShape_setScale_helper c
    
    {-# INLINABLE setOrigin #-}
    setOrigin c o = with o $ sfRectangleShape_setOrigin_helper c
    
    {-# INLINABLE getPosition #-}
    getPosition c = alloca $ \ptr -> sfRectangleShape_getPosition_helper c ptr >> peek ptr
    
    {-# INLINABLE getRotation #-}
    getRotation = sfRectangleShape_getRotation >=> return . realToFrac
    
    {-# INLINABLE getScale #-}
    getScale c = alloca $ \ptr -> sfRectangleShape_getScale_helper c ptr >> peek ptr
    
    {-# INLINABLE getOrigin #-}
    getOrigin c = alloca $ \ptr -> sfRectangleShape_getOrigin_helper c ptr >> peek ptr
    
    {-# INLINABLE move #-}
    move c off = with off $ sfRectangleShape_move_helper c
    
    {-# INLINABLE rotate #-}
    rotate r a = sfRectangleShape_rotate r (realToFrac a)
    
    {-# INLINABLE scale #-}
    scale c s = with s $ sfRectangleShape_scale_helper c
    
    {-# INLINABLE getTransform #-}
    getTransform c = alloca $ \ptr -> sfRectangleShape_getTransform_helper c ptr >> peek ptr
    
    {-# INLINABLE getInverseTransform #-}
    getInverseTransform c = alloca $ \ptr -> sfRectangleShape_getInverseTransform_helper c ptr >> peek ptr


foreign import ccall unsafe "sfRectangleShape_setPosition_helper"
    sfRectangleShape_setPosition_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setPosition(sfRectangleShape* shape, sfVector2f position);

foreign import ccall unsafe "sfRectangleShape_setRotation"
    sfRectangleShape_setRotation :: RectangleShape -> CFloat -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setRotation(sfRectangleShape* shape, float angle);

foreign import ccall unsafe "sfRectangleShape_setScale_helper"
    sfRectangleShape_setScale_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setScale(sfRectangleShape* shape, sfVector2f scale);

foreign import ccall unsafe "sfRectangleShape_setOrigin_helper"
    sfRectangleShape_setOrigin_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setOrigin(sfRectangleShape* shape, sfVector2f origin);

foreign import ccall unsafe "sfRectangleShape_getPosition_helper"
    sfRectangleShape_getPosition_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfRectangleShape_getPosition(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getRotation"
    sfRectangleShape_getRotation :: RectangleShape -> IO CFloat

--CSFML_GRAPHICS_API float sfRectangleShape_getRotation(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getScale_helper"
    sfRectangleShape_getScale_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfRectangleShape_getScale(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getOrigin_helper"
    sfRectangleShape_getOrigin_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfRectangleShape_getOrigin(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_move_helper"
    sfRectangleShape_move_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_move(sfRectangleShape* shape, sfVector2f offset);

foreign import ccall unsafe "sfRectangleShape_rotate"
    sfRectangleShape_rotate :: RectangleShape -> CFloat -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_rotate(sfRectangleShape* shape, float angle);

foreign import ccall unsafe "sfRectangleShape_scale_helper"
    sfRectangleShape_scale_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_scale(sfRectangleShape* shape, sfVector2f factors);

foreign import ccall unsafe "sfRectangleShape_getTransform_helper"
    sfRectangleShape_getTransform_helper :: RectangleShape -> Ptr Transform -> IO ()

--CSFML_GRAPHICS_API sfTransform sfRectangleShape_getTransform(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getInverseTransform_helper"
    sfRectangleShape_getInverseTransform_helper :: RectangleShape -> Ptr Transform -> IO ()

--CSFML_GRAPHICS_API sfTransform sfRectangleShape_getInverseTransform(const sfRectangleShape* shape);

instance SFTexturable RectangleShape where
    
    {-# INLINABLE setTexture #-}
    setTexture c tex reset = sfRectangleShape_setTexture c tex (fromIntegral . fromEnum $ reset)
    
    {-# INLINABLE setTextureRect #-}
    setTextureRect c rect = with rect $ sfRectangleShape_setTextureRect_helper c
    
    {-# INLINABLE getTexture #-}
    getTexture = sfRectangleShape_getTexture >=> return . checkNullTexture
    
    {-# INLINABLE getTextureRect #-}
    getTextureRect c = alloca $ \ptr -> sfRectangleShape_getTextureRect_helper c ptr >> peek ptr


foreign import ccall unsafe "sfRectangleShape_setTexture"
    sfRectangleShape_setTexture :: RectangleShape -> Texture -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setTexture(sfRectangleShape* shape, const sfTexture* texture, sfBool resetRect);

foreign import ccall unsafe "sfRectangleShape_setTextureRect_helper"
    sfRectangleShape_setTextureRect_helper :: RectangleShape -> Ptr IntRect -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setTextureRect(sfRectangleShape* shape, sfIntRect rect);

foreign import ccall unsafe "sfRectangleShape_getTexture"
    sfRectangleShape_getTexture :: RectangleShape -> IO Texture

--CSFML_GRAPHICS_API const sfTexture* sfRectangleShape_getTexture(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getTextureRect_helper"
    sfRectangleShape_getTextureRect_helper :: RectangleShape -> Ptr IntRect -> IO ()

--CSFML_GRAPHICS_API sfIntRect sfRectangleShape_getTextureRect(const sfRectangleShape* shape);

instance SFShape RectangleShape where
    
    {-# INLINABLE setFillColor #-}
    setFillColor c color = with color $ sfRectangleShape_setFillColor_helper c
    
    {-# INLINABLE setOutlineColor #-}
    setOutlineColor c color = with color $ sfRectangleShape_setOutlineColor_helper c
    
    {-# INLINABLE setOutlineThickness #-}
    setOutlineThickness r t = sfRectangleShape_setOutlineThickness r (realToFrac t)
    
    {-# INLINABLE getFillColor #-}
    getFillColor c = alloca $ \ptr -> sfRectangleShape_getFillColor_helper c ptr >> peek ptr
    
    {-# INLINABLE getOutlineColor #-}
    getOutlineColor c = alloca $ \ptr -> sfRectangleShape_getOutlineColor_helper c ptr >> peek ptr
    
    {-# INLINABLE getOutlineThickness #-}
    getOutlineThickness = sfRectangleShape_getOutlineThickness >=> return . realToFrac
    
    {-# INLINABLE getPointCount #-}
    getPointCount = sfRectangleShape_getPointCount >=> return . fromIntegral
    
    {-# INLINABLE getPoint #-}
    getPoint c idx = alloca $ \ptr -> sfRectangleShape_getPoint_helper c (fromIntegral idx) ptr >> peek ptr


foreign import ccall unsafe "sfRectangleShape_setFillColor_helper"
    sfRectangleShape_setFillColor_helper :: RectangleShape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setFillColor(sfRectangleShape* shape, sfColor color);

foreign import ccall unsafe "sfRectangleShape_setOutlineColor_helper"
    sfRectangleShape_setOutlineColor_helper :: RectangleShape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setOutlineColor(sfRectangleShape* shape, sfColor color);

foreign import ccall unsafe "sfRectangleShape_setOutlineThickness"
    sfRectangleShape_setOutlineThickness :: RectangleShape -> CFloat -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setOutlineThickness(sfRectangleShape* shape, float thickness);

foreign import ccall unsafe "sfRectangleShape_getFillColor_helper"
    sfRectangleShape_getFillColor_helper :: RectangleShape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API sfColor sfRectangleShape_getFillColor(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getOutlineColor_helper"
    sfRectangleShape_getOutlineColor_helper :: RectangleShape -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API sfColor sfRectangleShape_getOutlineColor(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getOutlineThickness"
    sfRectangleShape_getOutlineThickness :: RectangleShape -> IO CFloat

--CSFML_GRAPHICS_API float sfRectangleShape_getOutlineThickness(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getPointCount"
    sfRectangleShape_getPointCount :: RectangleShape -> IO CUInt

--CSFML_GRAPHICS_API unsigned int sfRectangleShape_getPointCount(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getPoint_helper"
    sfRectangleShape_getPoint_helper :: RectangleShape -> CUInt -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfRectangleShape_getPoint(const sfRectangleShape* shape, unsigned int index);


-- | Set the size of a rectangle shape.
setSize :: RectangleShape -> Vec2f -> IO ()
setSize rs s = with s $ sfRectangleShape_setSize_helper rs

foreign import ccall unsafe "sfRectangleShape_setSize_helper"
    sfRectangleShape_setSize_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfRectangleShape_setSize(sfRectangleShape* shape, sfVector2f size);


-- | Get the size of a rectangle shape.
getSize :: RectangleShape -> IO Vec2f
getSize r = alloca $ \ptr -> sfRectangleShape_getSize_helper r ptr >> peek ptr

foreign import ccall unsafe "sfRectangleShape_getSize_helper"
    sfRectangleShape_getSize_helper :: RectangleShape -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfRectangleShape_getSize(const sfRectangleShape* shape);


instance SFBoundable RectangleShape where

    {-# INLINABLE getLocalBounds #-}
    getLocalBounds c = alloca $ \ptr -> sfRectangleShape_getLocalBounds_helper c ptr >> peek ptr
    
    {-# INLINABLE getGlobalBounds #-}
    getGlobalBounds c = alloca $ \ptr -> sfRectangleShape_getGlobalBounds_helper c ptr >> peek ptr

foreign import ccall unsafe "sfRectangleShape_getLocalBounds_helper"
    sfRectangleShape_getLocalBounds_helper :: RectangleShape -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfRectangleShape_getLocalBounds(const sfRectangleShape* shape);

foreign import ccall unsafe "sfRectangleShape_getGlobalBounds_helper"
    sfRectangleShape_getGlobalBounds_helper :: RectangleShape -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfRectangleShape_getGlobalBounds(const sfRectangleShape* shape);

