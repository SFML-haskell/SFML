{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.System.Vector3
(
    Vec3f(..)
)
where


import Foreign.C.Types
import Foreign.Storable

#include <SFML/System/Vector3.h>


sizeFloat = #{size float}


data Vec3f = Vec3f {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float deriving Show


instance Storable Vec3f where
    sizeOf _ = 3*sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        x <- #{peek sfVector3f, x} ptr
        y <- #{peek sfVector3f, y} ptr
        z <- #{peek sfVector3f, z} ptr
        return $ Vec3f x y z
    
    poke ptr (Vec3f x y z) = do
        #{poke sfVector3f, x} ptr x
        #{poke sfVector3f, y} ptr y
        #{poke sfVector3f, z} ptr z

