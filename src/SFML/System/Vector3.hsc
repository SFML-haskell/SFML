{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.System.Vector3
(
    Vec3f(..)
)
where


import Control.Applicative ((<$>), (<*>))
import Foreign.C.Types
import Foreign.Storable

#include <SFML/System/Vector3.h>


sizeFloat = #{size float}


data Vec3f = Vec3f {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float deriving Show


instance Storable Vec3f where
    sizeOf _ = 3*sizeFloat
    alignment _ = alignment (undefined :: CFloat)

    peek ptr = Vec3f
            <$> fmap realToFrac (#{peek sfVector3f, x} ptr :: IO CFloat)
            <*> fmap realToFrac (#{peek sfVector3f, y} ptr :: IO CFloat)
            <*> fmap realToFrac (#{peek sfVector3f, z} ptr :: IO CFloat)

    poke ptr (Vec3f x y z) = do
        #{poke sfVector3f, x} ptr (realToFrac x :: CFloat)
        #{poke sfVector3f, y} ptr (realToFrac y :: CFloat)
        #{poke sfVector3f, z} ptr (realToFrac z :: CFloat)

