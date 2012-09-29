module SFML.Window.Types
(
    SFContext(..)
,   SFWindow(..)
)
where


import Foreign.Ptr


newtype SFContext = SFContext (Ptr SFContext)
newtype SFWindow  = SFWindow  (Ptr SFWindow)

