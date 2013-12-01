module SFML.Audio.SFSoundBuffer
where


import SFML.System.Time (Time)


class SFSoundBuffer a where
    
    -- | Get the number of channels used by a sound buffer.
    --
    -- If the sound is mono then the number of channels will
    -- be 1, 2 for stereo, etc.
    getChannelCount :: a -> IO Int
    
    -- | Get the total duration of a sound buffer.
    getDuration :: a -> IO Time

