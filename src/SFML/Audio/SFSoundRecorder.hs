module SFML.Audio.SFSoundRecorder
where


class SFSoundRecorder a where

    -- | Start the capture of a sound recorder.
    --
    -- The sample rate parameter defines the number of audio samples
    -- captured per second. The higher, the better the quality
    -- (for example, 44100 samples/sec is CD quality).
    --
    -- This function uses its own thread so that it doesn't block
    -- the rest of the program while the capture runs.
    --
    -- Please note that only one capture can happen at the same time.
    --
    -- Return 'True' if start of capture was successful, 'False' otherwise.
    startRecording
        :: a
        -> Int -- ^ Desired capture rate, in number of samples per second
        -> IO Bool

    -- | Stop the capture of a sound recorder.
    stopRecording :: a -> IO ()

