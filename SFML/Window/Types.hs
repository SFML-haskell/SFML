module SFML.Window.Types
(
    Context(..)
,   SFWindow(..)
)
where


import Foreign.Ptr


newtype Context = Context (Ptr Context)
newtype SFWindow  = SFWindow  (Ptr SFWindow)

