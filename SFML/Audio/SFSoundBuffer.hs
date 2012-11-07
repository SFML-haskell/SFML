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
    
    -- | Get the sample rate of a sound buffer.
    --
    -- The sample rate is the number of samples played per second.
    -- The higher, the better the quality (for example, 44100
    -- samples/s is CD quality).
    getSampleRate :: a -> IO Int

