module SFML.Graphics.RenderTarget
where


import SFML.Graphics.PrimitiveType
import SFML.Graphics.RenderStates
import SFML.Graphics.Types
import SFML.Graphics.Vertex

import Foreign.Ptr (Ptr)


class RenderTarget a where
    
    -- | Draw a sprite to the render-target.
    drawSprite
        :: a
        -> Sprite -- ^ Sprite to draw
        -> Maybe RenderStates -- ^ Render states to use for drawing ('Nothing' to use the default states)
        -> IO ()
    
    -- | Draw text to the render-target.
    drawText
        :: a
        -> Text -- ^ Text to draw
        -> Maybe RenderStates -- ^ Render states to use for drawing ('Nothing' to use the default states)
        -> IO ()
    
    -- | Draw a sprite to the render-target.
    drawShape
        :: a
        -> Shape -- ^ Shape to draw
        -> Maybe RenderStates -- ^ Render states to use for drawing ('Nothing' to use the default states)
        -> IO ()
    
    -- | Draw a sprite to the render-target.
    drawCircle
        :: a
        -> CircleShape -- ^ CircleShape to draw
        -> Maybe RenderStates -- ^ Render states to use for drawing ('Nothing' to use the default states)
        -> IO ()
    
    -- | Draw a sprite to the render-target.
    drawConvexShape
        :: a
        -> ConvexShape -- ^ ConvexShape to draw
        -> Maybe RenderStates -- ^ Render states to use for drawing ('Nothing' to use the default states)
        -> IO ()
        
    -- | Draw a sprite to the render-target.
    drawRectangle
        :: a
        -> RectangleShape -- ^ RectangleShape to draw
        -> Maybe RenderStates -- ^ Render states to use for drawing ('Nothing' to use the default states)
        -> IO ()
    
    -- | Draw a sprite to the render-target.
    drawVertexArray
        :: a
        -> VertexArray -- ^ VertexArray to draw
        -> Maybe RenderStates -- ^ Render states to use for drawing ('Nothing' to use the default states)
        -> IO ()
    
    -- | Draw primitives defined by an array of vertices to a render texture.
    drawPrimitives
        :: a
        -> [Vertex] -- ^ Vertices to render
        -> PrimitiveType -- ^ Type of primitives to draw
        -> Maybe RenderStates -- ^ Render states to use for drawing ('Nothing' to use the default states)
        -> IO ()
    
    drawPrimitives'
        :: a
        -> Ptr Vertex -- ^ Pointer to the vertices
        -> Int -- ^ Number of vertices in the array
        -> PrimitiveType -- ^ Type of primitives to draw
        -> Maybe RenderStates -- ^ Render states to use for drawing ('Nothing' to use the default states)
        -> IO ()
    
    -- | Save the current OpenGL render states and matrices.
    --
    -- This function can be used when you mix SFML drawing
    -- and direct OpenGL rendering. Combined with popGLStates,
    -- it ensures that:
    --
    -- * SFML's internal states are not messed up by your OpenGL code
    --
    -- * Your OpenGL states are not modified by a call to a SFML function
    --
    -- Note that this function is quite expensive: it saves all the
    -- possible OpenGL states and matrices, even the ones you
    -- don't care about. Therefore it should be used wisely.
    -- It is provided for convenience, but the best results will
    -- be achieved if you handle OpenGL states yourself (because
    -- you know which states have really changed, and need to be
    -- saved and restored). Take a look at the resetGLStates
    -- function if you do so.
    pushGLStates :: a -> IO ()
    
    -- | Restore the previously saved OpenGL render states and matrices.
    --
    -- See the description of pushGLStates to get a detailed
    -- description of these functions.
    popGLStates :: a -> IO ()
    
    -- | Reset the internal OpenGL states so that the target is ready for drawing
    --
    -- This function can be used when you mix SFML drawing
    -- and direct OpenGL rendering, if you choose not to use
    -- pushGLStates or popGLStates. It makes sure that all OpenGL
    -- states needed by SFML are set, so that subsequent sfa_draw*()
    -- calls will work as expected.
    resetGLStates :: a -> IO ()

