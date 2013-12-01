{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Graphics.RenderStates
(
    RenderStates(..)
,   renderStates
)
where


import SFML.Graphics.BlendMode
import SFML.Graphics.Transform
import SFML.Graphics.Types

import Foreign.C.Types (CFloat)
import Foreign.Ptr (nullPtr)
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
        #{poke sfRenderStates, blendMode} ptr  bm
        #{poke sfRenderStates, transform} ptr tr
        #{poke sfRenderStates, texture} ptr tx
        #{poke sfRenderStates, shader} ptr sh


size_sfRenderStates = #{size sfRenderStates}

-- | Default render states, defined as
--
-- @
-- renderStates = RenderStates 'BlendAlpha' 'idTransform' (Texture 'nullPtr') (Shader 'nullPtr')
-- @
--
-- This constant tries to mimic the C++ RenderStates default constructor to ease
-- the construction of render states. For example, instead of typing
--
-- @
-- states = RenderStates BlendAlpha idTransform tex (Shader nullptr)
-- @
--
-- Now we can simply type
--
-- @
-- states = renderStates { texture = tex }
-- @
renderStates = RenderStates BlendAlpha idTransform (Texture nullPtr) (Shader nullPtr)

{-typedef struct
{
    sfBlendMode      blendMode; ///< Blending mode
    sfTransform      transform; ///< Transform
    const sfTexture* texture;   ///< Texture
    const sfShader*  shader;    ///< Shader
} sfRenderStates;-}
