module SFML.Graphics.View
(
    createView
,   viewFromRect
,   copyView
,   destroyView
,   setViewCenter
,   setViewSize
,   setViewRotation
,   setViewport
,   resetView
,   getViewCenter
,   getViewSize
,   getViewRotation
,   getViewport
,   moveView
,   rotateView
,   zoomView
)
where


import SFML.Graphics.Rect
import SFML.Graphics.Types
import SFML.System.Vector2

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable (peek)


-- | Create a default view.
--
-- This function creates a default view of (0, 0, 1000, 1000)
createView :: IO View
createView = sfView_create

foreign import ccall unsafe "sfView_create"
    sfView_create :: IO View

--CSFML_GRAPHICS_API sfView* sfView_create(void);


-- | Construct a view from a rectangle
viewFromRect
    :: FloatRect -- ^ Rectangle defining the zone to display
    -> IO View

viewFromRect rect = with rect sfView_createFromRect

foreign import ccall unsafe "sfView_createFromRect_helper"
    sfView_createFromRect :: Ptr FloatRect -> IO View

--CSFML_GRAPHICS_API sfView* sfView_createFromRect(sfFloatRect rectangle);


-- | Copy an existing view.
copyView :: View -> IO View
copyView = sfView_copy

foreign import ccall unsafe "sfView_copy"
    sfView_copy :: View -> IO View

--CSFML_GRAPHICS_API sfView* sfView_copy(sfView* view);


-- | Destroy an existing view.
destroyView :: View -> IO ()
destroyView = sfView_destroy

foreign import ccall unsafe "sfView_destroy"
    sfView_destroy :: View -> IO ()

--CSFML_GRAPHICS_API void sfView_destroy(sfView* view);


-- | Set the center of a view.
setViewCenter :: View -> Vec2f -> IO ()
setViewCenter view center = with center $ sfView_setCenter_helper view

foreign import ccall unsafe "sfView_setCenter_helper"
    sfView_setCenter_helper :: View -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfView_setCenter(sfView* view, sfVector2f center);


-- | Set the size of a view.
setViewSize :: View -> Vec2f -> IO ()
setViewSize view size = with size $ sfView_setSize_helper view

foreign import ccall unsafe "sfView_setSize_helper"
    sfView_setSize_helper :: View -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfView_setSize(sfView* view, sfVector2f size);


-- | Set the orientation of a view.
--
-- The default rotation of a view is 0 degrees.
setViewRotation
    :: View  -- ^ View object
    -> Float -- ^ New angle, in degrees
    -> IO ()

setViewRotation = sfView_setRotation

foreign import ccall unsafe "sfView_setRotation"
    sfView_setRotation :: View -> Float -> IO ()

--CSFML_GRAPHICS_API void sfView_setRotation(sfView* view, float angle);


-- | Set the target viewport of a view
--
-- The viewport is the rectangle into which the contents of the
-- view are displayed, expressed as a factor (between 0 and 1)
-- of the size of the render target to which the view is applied.
-- For example, a view which takes the left side of the target would
-- be defined by a rect of (0, 0, 0.5, 1).
--
-- By default, a view has a viewport which covers the entire target.
setViewport
    :: View -- ^ View object
    -> FloatRect -- ^ New viewport rectangle
    -> IO ()

setViewport view rect = with rect $ sfView_setViewport_helper view

foreign import ccall unsafe "sfView_setViewport_helper"
    sfView_setViewport_helper :: View -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API void sfView_setViewport(sfView* view, sfFloatRect viewport);


-- | Reset a view to the given rectangle.
--
-- Note that this function resets the rotation angle to 0.
resetView
    :: View -- ^ View object
    -> FloatRect -- ^ Rectangle defining the zone to display
    -> IO ()

resetView view rect = with rect $ sfView_reset_helper view

foreign import ccall unsafe "sfView_reset_helper"
    sfView_reset_helper :: View -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API void sfView_reset(sfView* view, sfFloatRect rectangle);


-- | Get the center of a view.
getViewCenter :: View -> IO Vec2f
getViewCenter view = alloca $ \ptr -> sfView_getCenter view ptr >> peek ptr

foreign import ccall unsafe "sfView_getCenter"
    sfView_getCenter :: View -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfView_getCenter(const sfView* view);


-- | Get the size of a view.
getViewSize :: View -> IO Vec2f
getViewSize view = alloca $ \ptr -> sfView_getSize_helper view ptr >> peek ptr

foreign import ccall unsafe "sfView_getSize_helper"
    sfView_getSize_helper :: View -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfView_getSize(const sfView* view);


-- | Get the current orientation of a view, in degrees.
getViewRotation :: View -> IO Float
getViewRotation = sfView_getRotation

foreign import ccall unsafe "sfView_getRotation"
    sfView_getRotation :: View -> IO Float

--CSFML_GRAPHICS_API float sfView_getRotation(const sfView* view);


-- | Get the target viewport rectangle of a view, expressed as a factor of the target size.
getViewport :: View -> IO FloatRect
getViewport view = alloca $ \ptr -> sfView_getViewport_helper view ptr >> peek ptr

foreign import ccall unsafe "sfView_getViewport_helper"
    sfView_getViewport_helper :: View -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfView_getViewport(const sfView* view);


-- | Move a view relatively to its current position.
moveView
    :: View  -- ^ View object
    -> Vec2f -- ^ Offset
    -> IO ()

moveView view pos = with pos $ sfView_move_helper view

foreign import ccall unsafe "sfView_move_helper"
    sfView_move_helper :: View -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfView_move(sfView* view, sfVector2f offset);


-- | Rotate a view relatively to its current orientation.
rotateView
    :: View  -- ^ View object
    -> Float -- ^ Angle to rotate, in degrees
    -> IO ()

rotateView = sfView_rotate

foreign import ccall unsafe "sfView_rotate"
    sfView_rotate :: View -> Float -> IO ()

--CSFML_GRAPHICS_API void sfView_rotate(sfView* view, float angle);


-- | Resize a view rectangle relatively to its current size
--
-- Resizing the view simulates a zoom, as the zone displayed on
-- screen grows or shrinks.
--
-- factor is a multiplier:
--
-- * 1 keeps the size unchanged
--
-- * > 1 makes the view bigger (objects appear smaller)
--
-- * < 1 makes the view smaller (objects appear bigger)
zoomView
    :: View  -- ^ View object
    -> Float -- ^ Zoom factor to apply
    -> IO ()

zoomView = sfView_zoom

foreign import ccall unsafe "sfView_zoom"
    sfView_zoom :: View -> Float -> IO ()

--CSFML_GRAPHICS_API void sfView_zoom(sfView* view, float factor);

