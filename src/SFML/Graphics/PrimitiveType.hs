module SFML.Graphics.PrimitiveType
(
    PrimitiveType(..)
)
where


import Foreign.C.Types (CInt)
import Foreign.Ptr
import Foreign.Storable


-- | Types of primitives that a sf::VertexArray can render.
--
-- Points and lines have no area, therefore their thickness
-- will always be 1 pixel, regardless of the current transform
-- and view.
data PrimitiveType
    = Points        -- ^ List of individual points
    | Lines         -- ^ List of individual lines
    | LineStrip     -- ^ List of connected lines, a point uses the previous point to form a line
    | Triangles     -- ^ List of individual triangles
    | TriangleStrip -- ^ List of connected triangles, a point uses the two previous points to form a triangle
    | TriangleFan   -- ^ List of connected triangles, a point uses the common center and the previous point to form a triangle
    | Quads         -- ^ List of individual quads
    deriving (Eq, Enum, Bounded, Show)


instance Storable PrimitiveType where
    sizeOf _ = sizeOf (undefined :: CInt)
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = peek (castPtr ptr :: Ptr CInt) >>= return . toEnum . fromIntegral
    poke ptr pt = poke (castPtr ptr :: Ptr CInt) (fromIntegral . fromEnum $ pt)


{-typedef enum 
{
    sfPoints,         
    sfLines,          
    sfLinesStrip,     
    sfTriangles,      
    sfTrianglesStrip, 
    sfTrianglesFan,   
    sfQuads           
} sfPrimitiveType;-}

