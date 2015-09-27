module SFML.Graphics.SFDrawable where

import SFML.Graphics.CircleShape
import SFML.Graphics.ConvexShape
import SFML.Graphics.RenderStates
import SFML.Graphics.Shape
import SFML.Graphics.SFRenderTarget
import SFML.Graphics.Text
import SFML.Graphics.Types
import SFML.Graphics.VertexArray

class SFDrawable a where
    draw :: SFRenderTarget t => t -> a -> Maybe RenderStates -> IO ()

instance SFDrawable Sprite where
    draw = drawSprite

instance SFDrawable Text where
    draw = drawText

instance SFDrawable Shape where
    draw = drawShape

instance SFDrawable CircleShape where
    draw = drawCircle

instance SFDrawable ConvexShape where
    draw = drawConvexShape

instance SFDrawable VertexArray where
    draw = drawVertexArray
