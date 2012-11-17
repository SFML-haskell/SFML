{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.System.Vector2
(
    Vec2i(..)
,   Vec2u(..)
,   Vec2f(..)
)
where


import Data.Word
import Foreign.C.Types
import Foreign.Storable

#include <SFML/System/Vector2.h>


sizeFloat = #{size float}
sizeInt   = #{size int}


data Vec2i = Vec2i {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving Show


instance Storable Vec2i where
    sizeOf _ = 2*sizeInt
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = do
        x <- #{peek sfVector2i, x} ptr :: IO CInt
        y <- #{peek sfVector2i, y} ptr :: IO CInt
        return $ Vec2i (fromIntegral x) (fromIntegral y)
    
    poke ptr (Vec2i x y) = do
        #{poke sfVector2i, x} ptr (fromIntegral x :: CInt)
        #{poke sfVector2i, y} ptr (fromIntegral y :: CInt)


instance Num Vec2i where
    Vec2i ax ay + Vec2i bx by = Vec2i (ax + bx) (ay + by)
    Vec2i ax ay - Vec2i bx by = Vec2i (ax - bx) (ay - by)
    Vec2i ax ay * Vec2i bx by = Vec2i (ax * bx) (ay * by)
    abs (Vec2i ax ay) = Vec2i (abs ax) (abs ay)
    signum (Vec2i ax ay) = Vec2i (signum ax) (signum ay)
    fromInteger i = Vec2i i' i' where i' = fromInteger i


data Vec2u = Vec2u {-# UNPACK #-} !Word {-# UNPACK #-} !Word deriving Show


instance Storable Vec2u where
    sizeOf _ = 2*sizeInt
    alignment _ = alignment (undefined :: CUInt)
    
    peek ptr = do
        x <- #{peek sfVector2u, x} ptr :: IO CUInt
        y <- #{peek sfVector2u, y} ptr :: IO CUInt
        return $ Vec2u (fromIntegral x) (fromIntegral y)
    
    poke ptr (Vec2u x y) = do
        #{poke sfVector2u, x} ptr (fromIntegral x :: CUInt)
        #{poke sfVector2u, y} ptr (fromIntegral y :: CUInt)


instance Num Vec2u where
    Vec2u ax ay + Vec2u bx by = Vec2u (ax + bx) (ay + by)
    Vec2u ax ay - Vec2u bx by = Vec2u (ax - bx) (ay - by)
    Vec2u ax ay * Vec2u bx by = Vec2u (ax * bx) (ay * by)
    abs (Vec2u ax ay) = Vec2u (abs ax) (abs ay)
    signum (Vec2u ax ay) = Vec2u (signum ax) (signum ay)
    fromInteger i = Vec2u i' i' where i' = fromInteger i


data Vec2f = Vec2f {-# UNPACK #-} !Float {-# UNPACK #-} !Float deriving Show


instance Storable Vec2f where
    sizeOf _ = 2*sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        x <- fmap realToFrac (#{peek sfVector2f, x} ptr :: IO CFloat)
        y <- fmap realToFrac (#{peek sfVector2f, y} ptr :: IO CFloat)
        return $ Vec2f x y
    
    poke ptr (Vec2f x y) = do
        #{poke sfVector2f, x} ptr (realToFrac x :: CFloat)
        #{poke sfVector2f, y} ptr (realToFrac y :: CFloat)


instance Num Vec2f where
    Vec2f ax ay + Vec2f bx by = Vec2f (ax + bx) (ay + by)
    Vec2f ax ay - Vec2f bx by = Vec2f (ax - bx) (ay - by)
    Vec2f ax ay * Vec2f bx by = Vec2f (ax * bx) (ay * by)
    abs (Vec2f ax ay) = Vec2f (abs ax) (abs ay)
    signum (Vec2f ax ay) = Vec2f (signum ax) (signum ay)
    fromInteger i = Vec2f i' i' where i' = fromInteger i


instance Fractional Vec2f where
    Vec2f ax ay / Vec2f bx by = Vec2f (ax / bx) (ay / by)
    fromRational r = Vec2f r' r' where r' = fromRational r

