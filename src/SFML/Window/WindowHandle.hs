module SFML.Window.WindowHandle
(
    WindowHandle(..)
)
where


import Foreign.Ptr (Ptr)


newtype WindowHandle = WindowHandle (Ptr WindowHandle)

