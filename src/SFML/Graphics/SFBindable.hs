module SFML.Graphics.SFBindable
where

class SFBindable a where
      -- | Bind the resource for rendering (activate it).
      --
      -- This function is not part of the graphics API, it mustn't be
      -- used when drawing SFML entities. It must be used only if you
      -- mix sfShader with OpenGL code.
      bind :: a -> IO ()
