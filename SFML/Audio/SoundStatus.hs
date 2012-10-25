module SFML.Audio.SoundStatus
where


-- | Enumeration of statuses for sounds and musics
data SoundStatus
    = Stopped -- ^ Sound or music is not playing
    | Paused  -- ^ Sound or music is paused
    | Playing -- ^ Sound or music is playing
    deriving (Eq, Enum, Bounded, Show)

