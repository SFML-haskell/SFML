{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Graphics.RenderStates
(
    RenderStates(..)
)
where


import SFML.Graphics.BlendMode
import SFML.Graphics.Transform
import SFML.Graphics.Types

import Foreign.C.Types (CFloat)
import Foreign.Storable

#include <SFML/Graphics/RenderStates.h>


-- | Define the states used for drawing to a RenderTarget.
data RenderStates = RenderStates
    { blendMode :: BlendMode
    , transform :: Transform
    , texture   :: Texture
    , shader    :: Shader
    }


instance Storable RenderStates where
    sizeOf _ = size_sfRenderStates
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        bm <- #{peek sfRenderStates, blendMode} ptr
        tr <- #{peek sfRenderStates, transform} ptr
        tx <- #{peek sfRenderStates, texture} ptr
        sh <- #{peek sfRenderStates, shader} ptr
        return $ RenderStates bm tr tx sh
    
    poke ptr (RenderStates bm tr tx sh) = do
        #{poke sfRenderStates, blendMode} ptr bm
        #{poke sfRenderStates, transform} ptr tr
        #{poke sfRenderStates, texture} ptr tx
        #{poke sfRenderStates, shader} ptr sh


size_sfRenderStates = #{size sfRenderStates}

{-typedef struct
{
    sfBlendMode      blendMode; ///< Blending mode
    sfTransform      transform; ///< Transform
    const sfTexture* texture;   ///< Texture
    const sfShader*  shader;    ///< Shader
} sfRenderStates;-}

