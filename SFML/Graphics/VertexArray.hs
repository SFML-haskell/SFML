module SFML.Graphics.VertexArray
(
    createVA
,   copyVA
,   destroy
,   getVertexCount
,   getVertex
,   clearVA
,   resizeVA
,   appendVA
,   setPrimitiveType
,   getPrimitiveType
,   getVABounds
)
where


import SFML.Graphics.PrimitiveType
import SFML.Graphics.Rect
import SFML.Graphics.Types
import SFML.Graphics.Vertex
import SFML.SFResource

import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable (peek)


-- | Create a new vertex array.
createVA :: IO VertexArray
createVA = sfVertexArray_create

foreign import ccall unsafe "sfVertexArray_create"
    sfVertexArray_create :: IO VertexArray

--CSFML_GRAPHICS_API sfVertexArray* sfVertexArray_create(void);


-- | Copy an existing vertex array.
copyVA :: VertexArray -> IO VertexArray
copyVA = sfVertexArray_copy

foreign import ccall unsafe "sfVertexArray_copy"
    sfVertexArray_copy :: VertexArray -> IO VertexArray

--CSFML_GRAPHICS_API sfVertexArray* sfVertexArray_copy(sfVertexArray* vertexArray);


instance SFResource VertexArray where
    
    {-# INLINABLE destroy #-}
    destroy = sfVertexArray_destroy

foreign import ccall unsafe "sfVertexArray_destroy"
    sfVertexArray_destroy :: VertexArray -> IO ()

--CSFML_GRAPHICS_API void sfVertexArray_destroy(sfVertexArray* vertexArray);


-- | Return the vertex count of a vertex array.
getVertexCount :: VertexArray -> IO Int
getVertexCount = sfVertexArray_getVertexCount >=> return . fromIntegral

foreign import ccall unsafe "sfVertexArray_getVertexCount"
    sfVertexArray_getVertexCount :: VertexArray -> IO CUInt

--CSFML_GRAPHICS_API unsigned int sfVertexArray_getVertexCount(const sfVertexArray* vertexArray);


-- | Return the ith vertex.

-- This function doesn't check the index; it must be in range
-- [0, vertex count - 1]. The behaviour is otherwise undefined.
getVertex :: VertexArray -> Int -> IO (Ptr Vertex)
getVertex va i = sfVertexArray_getVertex va (fromIntegral i)

foreign import ccall unsafe "sfVertexArray_getVertex"
    sfVertexArray_getVertex :: VertexArray -> CUInt -> IO (Ptr Vertex)

--CSFML_GRAPHICS_API sfVertex* sfVertexArray_getVertex(sfVertexArray* vertexArray, unsigned int index);


-- | Clear a vertex array.
--
-- This function removes all the vertices from the array.
-- It doesn't deallocate the corresponding memory, so that
-- adding new vertices after clearing doesn't involve
-- reallocating all the memory.
clearVA :: VertexArray -> IO ()
clearVA = sfVertexArray_clear

foreign import ccall unsafe "sfVertexArray_clear"
    sfVertexArray_clear :: VertexArray -> IO ()

--CSFML_GRAPHICS_API void sfVertexArray_clear(sfVertexArray* vertexArray);


-- | Resize the vertex array.
--
-- If vertex count is greater than the current size, the previous
-- vertices are kept and new (default-constructed) vertices are
-- added.
--
-- If vertex count is less than the current size, existing vertices
-- are removed from the array.
resizeVA
    :: VertexArray
    -> Int -- ^ Vertex count; New size of the array (number of vertices)
    -> IO ()

resizeVA va size = sfVertexArray_resize va (fromIntegral size)

foreign import ccall unsafe "sfVertexArray_resize"
    sfVertexArray_resize :: VertexArray -> CUInt -> IO ()

--CSFML_GRAPHICS_API void sfVertexArray_resize(sfVertexArray* vertexArray, unsigned int vertexCount);


-- | Add a vertex to a vertex array array.
appendVA :: VertexArray -> Vertex -> IO ()
appendVA va v = with v $ sfVertexArray_append_helper va

foreign import ccall unsafe "sfVertexArray_append_helper"
    sfVertexArray_append_helper :: VertexArray -> Ptr Vertex -> IO ()

--CSFML_GRAPHICS_API void sfVertexArray_append(sfVertexArray* vertexArray, sfVertex vertex);


-- | Set the type of primitives of a vertex array.
--
-- This function defines how the vertices must be interpreted
-- when it's time to draw them:
--
-- * As points
--
-- * As lines
--
-- * As triangles
--
-- * As quads
--
-- The default primitive type is sfPoints.
setPrimitiveType :: VertexArray -> PrimitiveType -> IO ()
setPrimitiveType va pt = sfVertexArray_setPrimitiveType va (fromIntegral . fromEnum $ pt)

foreign import ccall unsafe "sfVertexArray_setPrimitiveType"
    sfVertexArray_setPrimitiveType :: VertexArray -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfVertexArray_setPrimitiveType(sfVertexArray* vertexArray, sfPrimitiveType type);


-- | Get the type of primitives drawn by a vertex array.
getPrimitiveType :: VertexArray -> IO PrimitiveType
getPrimitiveType = sfVertexArray_getPrimitiveType >=> return . toEnum . fromIntegral

foreign import ccall unsafe "sfVertexArray_getPrimitiveType"
    sfVertexArray_getPrimitiveType :: VertexArray -> IO CInt

--CSFML_GRAPHICS_API sfPrimitiveType sfVertexArray_getPrimitiveType(sfVertexArray* vertexArray);


-- | Compute the bounding rectangle of a vertex array.
--
-- This function returns the axis-aligned rectangle that
-- contains all the vertices of the array.
getVABounds :: VertexArray -> IO FloatRect
getVABounds va = alloca $ \ptr -> sfVertexArray_getBounds_helper va ptr >> peek ptr

foreign import ccall unsafe "sfVertexArray_getBounds_helper"
    sfVertexArray_getBounds_helper :: VertexArray -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfVertexArray_getBounds(sfVertexArray* vertexArray);

