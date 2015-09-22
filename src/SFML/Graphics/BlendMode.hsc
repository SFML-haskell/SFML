module SFML.Graphics.BlendMode
(
    BlendFactor(..)
,   BlendEquation(..)
,   BlendMode(..)
,   blendAlpha
,   blendAdd
,   blendMultiply
,   blendNone
)
where


import Control.Applicative ((<$>), (<*>))
import Foreign.C.Types (CInt)
import Foreign.Ptr
import Foreign.Storable

#include <SFML/Graphics/BlendMode.h>


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t(y__); }, y__)


-- | Enumeration of the blending factors
data BlendFactor
    = BlendFactorZero             -- ^ (0, 0, 0, 0)
    | BlendFactorOne              -- ^ (1, 1, 1, 1)
    | BlendFactorSrcColor         -- ^ (src.r, src.g, src.b, src.a)
    | BlendFactorOneMinusSrcColor -- ^ (1, 1, 1, 1) - (src.r, src.g, src.b, src.a)
    | BlendFactorDstColor         -- ^ (dst.r, dst.g, dst.b, dst.a)
    | BlendFactorOneMinusDstColor -- ^ (1, 1, 1, 1) - (dst.r, dst.g, dst.b, dst.a)
    | BlendFactorSrcAlpha         -- ^ (src.a, src.a, src.a, src.a)
    | BlendFactorOneMinusSrcAlpha -- ^ (1, 1, 1, 1) - (src.a, src.a, src.a, src.a)
    | BlendFactorDstAlpha         -- ^ (dst.a, dst.a, dst.a, dst.a)
    | BlendFactorOneMinusDstAlpha -- ^ (1, 1, 1, 1) - (dst.a, dst.a, dst.a, dst.a)
    deriving (Eq, Enum, Bounded, Show)


-- | Enumeration of the blending equations
data BlendEquation
    = BlendEquationAdd      -- ^ Pixel = Src * SrcFactor + Dst * DstFactor
    | BlendEquationSubtract -- ^ Pixel = Src * SrcFactor - Dst * DstFactor
    deriving (Eq, Enum, Bounded, Show)


-- | Available blending modes for drawing.
data BlendMode
    = BlendMode
    { colorSrcFactor :: BlendFactor    -- ^ Source blending factor for the color channels
    , colorDstFactor :: BlendFactor    -- ^ Destination blending factor for the color channels
    , colorEquation  :: BlendEquation  -- ^ Blending equation for the color channels
    , alphaSrcFactor :: BlendFactor    -- ^ Source blending factor for the alpha channel
    , alphaDstFactor :: BlendFactor    -- ^ Destination blending factor for the alpha channel
    , alphaEquation  :: BlendEquation  -- ^ Blending equation for the alpha channel
    } deriving (Eq, Show)


instance Storable BlendFactor where
    sizeOf _ = #{size sfBlendFactor}
    alignment _ = #{alignment sfBlendFactor}

    peek ptr = fmap (toEnum . fromIntegral) $ peek (castPtr ptr :: Ptr CInt)
    poke ptr bm = poke (castPtr ptr :: Ptr CInt) (fromIntegral . fromEnum $ bm)


instance Storable BlendEquation where
    sizeOf _ = #{size sfBlendEquation}
    alignment _ = #{alignment sfBlendEquation}

    peek ptr = fmap (toEnum . fromIntegral) $ peek (castPtr ptr :: Ptr CInt)
    poke ptr bm = poke (castPtr ptr :: Ptr CInt) (fromIntegral . fromEnum $ bm)


instance Storable BlendMode where
    sizeOf _ = #{size sfBlendMode}
    alignment _ = #{alignment sfBlendMode}

    peek ptr = BlendMode <$> #{peek sfBlendMode, colorSrcFactor}  ptr
                         <*> #{peek sfBlendMode, colorDstFactor}  ptr
                         <*> #{peek sfBlendMode, colorEquation}   ptr
                         <*> #{peek sfBlendMode, alphaSrcFactor}  ptr
                         <*> #{peek sfBlendMode, alphaDstFactor}  ptr
                         <*> #{peek sfBlendMode, alphaEquation}   ptr
    poke ptr bm = do
        #{poke sfBlendMode, colorSrcFactor} ptr (colorSrcFactor bm)
        #{poke sfBlendMode, colorDstFactor} ptr (colorDstFactor bm)
        #{poke sfBlendMode, colorEquation}  ptr (colorEquation  bm)
        #{poke sfBlendMode, alphaSrcFactor} ptr (alphaSrcFactor bm)
        #{poke sfBlendMode, alphaDstFactor} ptr (alphaDstFactor bm)
        #{poke sfBlendMode, alphaEquation}  ptr (alphaEquation  bm)


blendAlpha = BlendMode BlendFactorSrcAlpha
                       BlendFactorOneMinusSrcAlpha
                       BlendEquationAdd
                       BlendFactorOne
                       BlendFactorOneMinusSrcAlpha
                       BlendEquationAdd


blendAdd = BlendMode BlendFactorSrcAlpha
                     BlendFactorOne
                     BlendEquationAdd
                     BlendFactorOne
                     BlendFactorOne
                     BlendEquationAdd


blendMultiply = BlendMode BlendFactorDstColor
                          BlendFactorZero
                          BlendEquationAdd
                          BlendFactorDstColor
                          BlendFactorZero
                          BlendEquationAdd


blendNone = BlendMode BlendFactorOne
                      BlendFactorZero
                      BlendEquationAdd
                      BlendFactorOne
                      BlendFactorZero
                      BlendEquationAdd
