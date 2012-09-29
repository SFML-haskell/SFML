module SFML.Window.WindowHandle
(
    SFWindowHandle(..)
)
where


import Foreign.Ptr (Ptr)


newtype SFWindowHandle = SFWindowHandle (Ptr SFWindowHandle)

