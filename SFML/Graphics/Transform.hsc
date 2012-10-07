{-# LANGUAGE CPP #-}
module SFML.Graphics.Transform
(
    Transform
,   m00, m10, m20
,   m01, m11, m21
,   m02, m12, m22
,   createTransform
,   idTransform
,   translation
,   rotation
,   rotationWithCenter
,   scaling
,   scalingWithCenter
,   inverse
,   fastInverse
,   transformPoint
,   transformDir
,   transformRect
)
where


import SFML.Graphics.Rect
import SFML.Graphics.Types
import SFML.System.Vector2

import Data.List (foldl')
import Foreign.C.Types
import Foreign.Storable


sizeFloat = #{size float}


-- | Encapsulate a 3x3 transform matrix.
data Transform = Transform
    { m00 :: !Float, m10 :: !Float, m20 :: !Float
    , m01 :: !Float, m11 :: !Float, m21 :: !Float
    , m02 :: !Float, m12 :: !Float, m22 :: !Float
    }


instance Storable Transform where
    sizeOf _ = 9 * sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        m00 <- peekByteOff ptr 0
        m01 <- peekByteOff ptr sizeFloat
        m02 <- peekByteOff ptr $ 2*sizeFloat
        m10 <- peekByteOff ptr $ 3*sizeFloat
        m11 <- peekByteOff ptr $ 4*sizeFloat
        m12 <- peekByteOff ptr $ 5*sizeFloat
        m20 <- peekByteOff ptr $ 6*sizeFloat
        m21 <- peekByteOff ptr $ 7*sizeFloat
        m22 <- peekByteOff ptr $ 8*sizeFloat
        return $ Transform m00 m10 m20 m01 m11 m21 m02 m12 m22
    
    poke ptr (Transform m00 m10 m20 m01 m11 m21 m02 m12 m22) = do
        pokeByteOff ptr 0 m00
        pokeByteOff ptr sizeFloat m01
        pokeByteOff ptr (2*sizeFloat) m02
        pokeByteOff ptr (3*sizeFloat) m10
        pokeByteOff ptr (4*sizeFloat) m11
        pokeByteOff ptr (5*sizeFloat) m12
        pokeByteOff ptr (6*sizeFloat) m20
        pokeByteOff ptr (7*sizeFloat) m21
        pokeByteOff ptr (8*sizeFloat) m22


instance Num Transform where
    (Transform a00 a01 a02 a03 a04 a05 a06 a07 a08)
        + (Transform b00 b01 b02 b03 b04 b05 b06 b07 b08)
            = Transform (a00 + b00) (a01 + b01) (a02 + b02)
                        (a03 + b03) (a04 + b04) (a05 + b05)
                        (a06 + b06) (a07 + b07) (a08 + b08)
    
    (Transform a00 a01 a02 a03 a04 a05 a06 a07 a08)
        - (Transform b00 b01 b02 b03 b04 b05 b06 b07 b08)
            = Transform (a00 - b00) (a01 - b01) (a02 - b02)
                        (a03 - b03) (a04 - b04) (a05 - b05)
                        (a06 - b06) (a07 - b07) (a08 - b08)
    
    (Transform a00 a10 a20 a01 a11 a21 a02 a12 a22)
        * (Transform b00 b10 b20 b01 b11 b21 b02 b12 b22)
            = Transform (a00 * b00 + a10 * b01 + a20 * b02)
                        (a00 * b10 + a10 * b11 + a20 * b12)
                        (a00 * b20 + a10 * b21 + a20 * b22)
                        
                        (a01 * b00 + a11 * b01 + a21 * b02)
                        (a01 * b10 + a11 * b11 + a21 * b12)
                        (a01 * b20 + a11 * b21 + a21 * b22)
                        
                        (a02 * b00 + a12 * b01 + a22 * b02)
                        (a02 * b10 + a12 * b11 + a22 * b12)
                        (a02 * b20 + a12 * b21 + a22 * b22)
    
    abs (Transform a00 a01 a02 a03 a04 a05 a06 a07 a08) =
        (Transform (abs a00) (abs a01) (abs a02) (abs a03)
            (abs a04) (abs a05) (abs a06) (abs a07) (abs a08))
    
    signum (Transform a00 a01 a02 a03 a04 a05 a06 a07 a08) =
        (Transform (signum a00) (signum a01) (signum a02) (signum a03)
            (signum a04) (signum a05) (signum a06) (signum a07) (signum a08))
    
    fromInteger i = Transform i' i' i' i' i' i' i' i' i' where i' = fromInteger i


-- | Create a new transform from a matrix.
createTransform
    :: Float -- ^ Element (0, 0) of the matrix
    -> Float -- ^ Element (0, 1) of the matrix
    -> Float -- ^ Element (0, 2) of the matrix
    -> Float -- ^ Element (1, 0) of the matrix
    -> Float -- ^ Element (1, 1) of the matrix
    -> Float -- ^ Element (1, 2) of the matrix
    -> Float -- ^ Element (2, 0) of the matrix
    -> Float -- ^ Element (2, 1) of the matrix
    -> Float -- ^ Element (2, 2) of the matrix
    -> Transform

createTransform = Transform


-- | Identity transform.
idTransform :: Transform
idTransform = Transform 1 0 0 0 1 0 0 0 1


-- | Create a translation.
translation
    :: Float -- ^ Offset to apply on X axis
    -> Float -- ^ Offset to apply on Y axis
    -> Transform

translation x y = Transform
    1 0 x
    0 1 y
    0 0 1

-- | Create a rotation.
rotation
    :: Float -- ^ Rotation angle in degrees
    -> Transform

rotation deg =
    let rad = deg * pi / 180
        sa  = sin rad
        ca  = cos rad
    in Transform
        ca (-sa) 0
        sa   ca  0
        0    0   1

-- | Create a rotation.
--
-- The center of rotation is provided for convenience as a second
-- argument, so that you can build rotations around arbitrary points
-- more easily (and efficiently) than the usual
-- [translate(-center), rotate(angle), translate(center)].
rotationWithCenter
    :: Float -- ^ Rotation angle, in degrees
    -> Float -- ^ X coordinate of the center of rotation
    -> Float -- ^ Y coordinate of the center of rotation
    -> Transform

rotationWithCenter deg x y =
    let rad = deg * pi / 180
        ca  = cos rad
        sa  = sin rad
    in Transform
        ca (-sa) (x * (1 - ca) + y * sa)
        sa   ca  (y * (1 - ca) - x * sa)
        0    0    1


-- | Create a scaling.
scaling
    :: Float -- ^ Scaling factor on the X axis
    -> Float -- ^ Scaling factor on the Y axis
    -> Transform

scaling x y = Transform
    x 0 0
    0 y 0
    0 0 1


-- | Create a scaling.
--
-- The center of scaling is provided for convenience as a second
-- argument, so that you can build scaling around arbitrary points
-- more easily (and efficiently) than the usual
-- [translate(-center), scale(factors), translate(center)]
scalingWithCenter
    :: Float -- ^ Scaling factor on X axis
    -> Float -- ^ Scaling factor on Y axis
    -> Float -- ^ X coordinate of the center of scaling
    -> Float -- ^ Y coordinate of the center of scaling
    -> Transform

scalingWithCenter sx sy cx cy = Transform
    sx 0  (cx * (1 - sx))
    0  sy (cy * (1 - sy))
    0  0   1


-- | Return the determinant of the transform.
determinant :: Transform -> Float
determinant (Transform m00 m10 m20 m01 m11 m21 m02 m12 m22)
    = m00 * (m11 * m22 - m12 * m21)
    + m01 * (m12 * m20 - m10 * m22)
    + m02 * (m10 * m21 - m11 * m20)


-- | Return the inverse of a transform.
--
-- If the inverse cannot be computed, a new identity transform
-- is returned.
inverse :: Transform -> Transform
inverse t@(Transform m00 m10 m20 m01 m11 m21 m02 m12 m22) =
    let det = determinant t
        det_inv = 1 / det
        t00 =   m11 * m22 - m12 * m21
        t01 = - m10 * m22 + m12 * m20
        t02 =   m10 * m21 - m11 * m20
        t10 = - m01 * m22 + m02 * m21
        t11 =   m00 * m22 - m02 * m20
        t12 = - m00 * m21 + m01 * m20
        t20 =   m01 * m12 - m02 * m11
        t21 = - m00 * m12 + m02 * m10
        t22 =   m00 * m11 - m01 * m10
    in
        if determinant t /= 0 then Transform
            (t00*det_inv) (t01*det_inv) (t02*det_inv)
            (t10*det_inv) (t11*det_inv) (t12*det_inv)
            (t20*det_inv) (t21*det_inv) (t22*det_inv)
        else
            idTransform


-- | Return the inverse of a transform.
--
-- This function is only applicable when the transform is composed
-- of rotations and translations only.
fastInverse :: Transform -> Transform
fastInverse mat =
    let rx = m00 mat
        ry = m01 mat
        fx = m10 mat
        fy = m11 mat
        tx = (-m20 mat)
        ty = (-m21 mat)
    in Transform
        rx ry (tx*rx + ty*ry)
        fx fy (tx*fx + ty*fy)
        0  0   1


-- | Apply a transform to a 2D point.
transformPoint :: Transform -> Vec2f -> Vec2f
transformPoint (Transform m00 m10 m20 m01 m11 m21 _ _ _) (Vec2f x y) = Vec2f x' y'
    where
        x' = m00 * x + m10 * y + m20
        y' = m01 * x + m11 * y + m21


-- | Apply a transform to a 2D direction vector.
transformDir :: Transform -> Vec2f -> Vec2f
transformDir (Transform m00 m10 _ m01 m11 _ _ _ _) (Vec2f x y) = Vec2f x' y'
    where
        x' = m00 * x + m10 * y
        y' = m01 * x + m11 * y


-- | Apply a transform to a rectangle.
--
-- Since SFML doesn't provide support for oriented rectangles,
-- the result of this function is always an axis-aligned
-- rectangle, which means that if the transform contains a
-- rotation, the bounding rectangle of the transformed rectangle
-- is returned.
transformRect :: Transform -> FloatRect -> FloatRect
transformRect transf (FloatRect l t w h) = FloatRect l' t' w' h'
    where
        p0@(Vec2f p0x p0y) = transformPoint transf (Vec2f l $ t)
        p1@(Vec2f p1x p1y) = transformPoint transf (Vec2f l $ t + h)
        p2@(Vec2f p2x p2y) = transformPoint transf (Vec2f (l + w) t)
        p3@(Vec2f p3x p3y) = transformPoint transf (Vec2f (l + w) (t + h))
        
        left   = p0x;
        top    = p0y;
        right  = p0x;
        bottom = p0y;
        
        l' = min p3x $ min p2x $ min p1x p0x
        t' = min p3y $ min p2y $ min p1y p0y
        r' = max p3x $ max p2x $ max p1x p0x
        b' = max p3y $ max p2y $ max p1y p0y
        w' = r' - l'
        h' = b' - t'

