module SFML.Audio.SFSampled
where


class SFSampled a where
    
    -- | Get the sample rate of a sound buffer.
    --
    -- The sample rate is the number of samples played per second.
    -- The higher, the better the quality (for example, 44100
    -- samples/s is CD quality).
    getSampleRate :: a -> IO Int

