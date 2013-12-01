{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Graphics.Vertex
(
    Vertex(..)
)
where


import SFML.Graphics.Color
import SFML.System.Vector2

import Foreign.C.Types (CFloat)
import Foreign.Storable

#include <SFML/Graphics/Vertex.h>


-- | Define a point with color and texture coordinates.
data Vertex = Vertex
    { position  :: Vec2f
    , color     :: Color
    , texCoords :: Vec2f
    }
    deriving (Show)


instance Storable Vertex where
    sizeOf _ = size_sfVertex
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        pos <- #{peek sfVertex, position} ptr
        col <- #{peek sfVertex, color} ptr
        tex <- #{peek sfVertex, texCoords} ptr
        return $ Vertex pos col tex
    
    poke ptr (Vertex pos col tex) = do
        #{poke sfVertex, position} ptr pos
        #{poke sfVertex, color} ptr col
        #{poke sfVertex, texCoords} ptr tex


size_sfVertex = #{size sfVertex}

{-typedef struct
{
    sfVector2f position;  ///< Position of the vertex
    sfColor    color;     ///< Color of the vertex
    sfVector2f texCoords; ///< Coordinates of the texture's pixel to map to the vertex
} sfVertex;-}

