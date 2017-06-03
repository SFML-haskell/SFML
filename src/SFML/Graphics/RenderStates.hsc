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

import Control.Applicative ((<$>), (<*>))
import Foreign.C.Types (CIntPtr)
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
    sizeOf _ = #{size sfRenderStates}
    alignment _ = #{alignment sfRenderStates}

    peek ptr = RenderStates
            <$> #{peek sfRenderStates, blendMode} ptr
            <*> #{peek sfRenderStates, transform} ptr
            <*> #{peek sfRenderStates, texture} ptr
            <*> #{peek sfRenderStates, shader} ptr

    poke ptr (RenderStates bm tr tx sh) = do
        #{poke sfRenderStates, blendMode} ptr  bm
        #{poke sfRenderStates, transform} ptr tr
        #{poke sfRenderStates, texture} ptr tx
        #{poke sfRenderStates, shader} ptr sh

-- | Default render states, defined as
--
-- @
-- renderStates = RenderStates 'blendAlpha' 'idTransform' (Texture 'nullPtr') (Shader 'nullPtr')
-- @
--
-- This constant tries to mimic the C++ RenderStates default constructor to ease
-- the construction of render states. For example, instead of typing
--
-- @
-- states = RenderStates blendAlpha idTransform tex (Shader nullptr)
-- @
--
-- Now we can simply type
--
-- @
-- states = renderStates { texture = tex }
-- @
renderStates = RenderStates blendAlpha idTransform (Texture nullPtr) (Shader nullPtr)

{-typedef struct
{
    sfBlendMode      blendMode; ///< Blending mode
    sfTransform      transform; ///< Transform
    const sfTexture* texture;   ///< Texture
    const sfShader*  shader;    ///< Shader
} sfRenderStates;-}
