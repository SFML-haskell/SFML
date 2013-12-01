{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Graphics.Rect
(
    FloatRect(..)
,   IntRect(..)
,   Rect(..)
,   floatRectContains
,   intRectContains
)
where


import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable
import System.IO.Unsafe

#include <SFML/Graphics/Rect.h>


sizeInt = #{size int}
sizeFloat = #{size float}


-- | Utility class for manipulating rectangles.
data FloatRect = FloatRect
    { fleft   :: Float
    , ftop    :: Float
    , fwidth  :: Float
    , fheight :: Float
    }


instance Storable FloatRect where
    sizeOf _ = 4 * sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        l <- fmap realToFrac (#{peek sfFloatRect, left} ptr :: IO CFloat)
        t <- fmap realToFrac (#{peek sfFloatRect, top} ptr :: IO CFloat)
        w <- fmap realToFrac (#{peek sfFloatRect, width} ptr :: IO CFloat)
        h <- fmap realToFrac (#{peek sfFloatRect, height} ptr :: IO CFloat)
        return $ FloatRect l t w h
    
    poke ptr (FloatRect l t w h) = do
        #{poke sfFloatRect, left} ptr (realToFrac l :: CFloat)
        #{poke sfFloatRect, top} ptr (realToFrac t :: CFloat)
        #{poke sfFloatRect, width} ptr (realToFrac w :: CFloat)
        #{poke sfFloatRect, height} ptr (realToFrac h :: CFloat)


-- | Utility class for manipulating rectangles.
data IntRect = IntRect
    { ileft   :: Int
    , itop    :: Int
    , iwidth  :: Int
    , iheight :: Int
    }


instance Storable IntRect where
    sizeOf _ = 4 * sizeInt
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = do
        l <- #{peek sfIntRect, left} ptr   :: IO CInt
        t <- #{peek sfIntRect, top} ptr    :: IO CInt
        w <- #{peek sfIntRect, width} ptr  :: IO CInt
        h <- #{peek sfIntRect, height} ptr :: IO CInt
        return $ IntRect (fromIntegral l) (fromIntegral t) (fromIntegral w) (fromIntegral h)
    
    poke ptr (IntRect l t w h) = do
        #{poke sfIntRect, left} ptr (fromIntegral l :: CInt)
        #{poke sfIntRect, top} ptr (fromIntegral t :: CInt)
        #{poke sfIntRect, width} ptr (fromIntegral w :: CInt)
        #{poke sfIntRect, height} ptr (fromIntegral h :: CInt)


-- | Check if a point is inside a rectangle's area.
floatRectContains
    :: Float -- ^ X coordinate of the point to test
    -> Float -- ^ Y coordinate of the point to test
    -> FloatRect -- ^ Rectangle to test
    -> Bool

floatRectContains x y r = unsafeDupablePerformIO $ fmap (/=0) . with r $ \ptr -> sfFloatRect_contains ptr x y

foreign import ccall unsafe "sfFloatRect_contains"
    sfFloatRect_contains :: Ptr FloatRect -> Float -> Float -> IO CInt

--CSFML_GRAPHICS_API sfBool sfFloatRect_contains(const sfFloatRect* rect, float x, float y);


-- | Check if a point is inside a rectangle's area.
intRectContains
    :: Int -- ^ X coordinate of the point to test
    -> Int -- ^ Y coordinate of the point to test
    -> IntRect -- ^ Rectangle to test
    -> Bool

intRectContains x y r = unsafeDupablePerformIO $ fmap (/=0) . with r $
    \ptr -> sfIntRect_contains ptr (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "sfIntRect_contains"
    sfIntRect_contains :: Ptr IntRect -> CInt -> CInt -> IO CInt

--CSFML_GRAPHICS_API sfBool sfIntRect_contains(const sfIntRect* rect, int x, int y);


class Rect a where
    -- | Check intersection between two rectangles.
    intersectRect
        :: a -- ^ First rectangle to test
        -> a -- ^ Second rectangle to test
        -> Maybe a -- ^ Overlapping rect


instance Rect FloatRect where
    
    intersectRect r1 r2 = unsafeDupablePerformIO $
        alloca $ \ptr1 ->
        alloca $ \ptr2 ->
        alloca $ \ptrOut -> do
        poke ptr1 r1
        poke ptr2 r2
        result <- sfFloatRect_intersects ptr1 ptr2 ptrOut
        case result of
            0 -> return Nothing
            _ -> peek ptrOut >>= return . Just


foreign import ccall unsafe "sfFloatRect_intersects"
    sfFloatRect_intersects :: Ptr FloatRect -> Ptr FloatRect -> Ptr FloatRect -> IO CInt

--CSFML_GRAPHICS_API sfBool sfFloatRect_intersects(const sfFloatRect* rect1, const sfFloatRect* rect2, sfFloatRect* intersection);


instance Rect IntRect where
    
    intersectRect r1 r2 = unsafeDupablePerformIO $
        alloca $ \ptr1 ->
        alloca $ \ptr2 ->
        alloca $ \ptrOut -> do
        poke ptr1 r1
        poke ptr2 r2
        result <- sfIntRect_intersects ptr1 ptr2 ptrOut
        case result of
            0 -> return Nothing
            _ -> peek ptrOut >>= return . Just


foreign import ccall unsafe "sfIntRect_intersects"
    sfIntRect_intersects :: Ptr IntRect -> Ptr IntRect -> Ptr IntRect -> IO CInt

--CSFML_GRAPHICS_API sfBool sfIntRect_intersects(const sfIntRect* rect1, const sfIntRect* rect2, sfIntRect* intersection);

