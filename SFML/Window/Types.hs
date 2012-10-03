module SFML.Window.Types
(
    Context(..)
,   Window(..)
)
where


import Foreign.Ptr


newtype Context = Context (Ptr Context)
newtype Window  = Window  (Ptr Window)

