module SFML.Graphics.SFViewable
where


import SFML.Graphics.Rect
import SFML.Graphics.Types


class SFViewable a where
    
    -- | Change the target's current active view.
    setView :: a -> View -> IO ()
    
    -- | Get the target's current active view.
    getView :: a -> IO View
    
    -- | Get the target's default view.
    getDefaultView :: a -> IO View
    
    -- | Get the viewport of a view applied to this target, expressed in pixels in the current target.
    getViewport :: a -> View -> IO IntRect

