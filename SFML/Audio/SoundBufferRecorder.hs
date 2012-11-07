module SFML.Audio.SoundBufferRecorder
(
    createSoundBufferRecorder
,   destroySoundBufferRecorder
,   startRecording
,   stopRecording
,   getSampleRate
,   getRecorderBuffer
)
where


import SFML.Audio.SFSampled
import SFML.Audio.SFSoundRecorder
import SFML.Audio.Types

import Control.Monad ((>=>))
import Foreign.C.Types
import Unsafe.Coerce (unsafeCoerce)


checkNull :: SoundBufferRecorder -> Maybe SoundBufferRecorder
checkNull sbr@(SoundBufferRecorder ptr) =
    case (unsafeCoerce ptr) of
        0 -> Nothing
        _ -> Just sbr


-- | Create a new sound buffer recorder.
createSoundBufferRecorder :: IO (Maybe SoundBufferRecorder)
createSoundBufferRecorder = fmap checkNull sfSoundBufferRecorder_create

foreign import ccall unsafe "sfSoundBufferRecorder_create"
    sfSoundBufferRecorder_create :: IO SoundBufferRecorder

-- \return A new sfSoundBufferRecorder object (NULL if failed)

-- CSFML_AUDIO_API sfSoundBufferRecorder* sfSoundBufferRecorder_create(void);


-- | Destroy a sound buffer recorder.
destroySoundBufferRecorder :: SoundBufferRecorder -> IO ()
destroySoundBufferRecorder = sfSoundBufferRecorder_destroy

foreign import ccall unsafe "sfSoundBufferRecorder_destroy"
    sfSoundBufferRecorder_destroy :: SoundBufferRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundBufferRecorder_destroy(sfSoundBufferRecorder* soundBufferRecorder);


instance SFSoundRecorder SoundBufferRecorder where
    
    startRecording rec rate = sfSoundBufferRecorder_start rec (fromIntegral rate)
    
    stopRecording = sfSoundBufferRecorder_stop


foreign import ccall unsafe "sfSoundBufferRecorder_start"
    sfSoundBufferRecorder_start :: SoundBufferRecorder -> CUInt -> IO ()

-- CSFML_AUDIO_API void sfSoundBufferRecorder_start(sfSoundBufferRecorder* soundBufferRecorder, unsigned int sampleRate);

foreign import ccall unsafe "sfSoundBufferRecorder_stop"
    sfSoundBufferRecorder_stop :: SoundBufferRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundBufferRecorder_stop(sfSoundBufferRecorder* soundBufferRecorder);


instance SFSampled SoundBufferRecorder where
    
    getSampleRate = sfSoundBufferRecorder_getSampleRate >=> return . fromIntegral


foreign import ccall unsafe "sfSoundBufferRecorder_getSampleRate"
    sfSoundBufferRecorder_getSampleRate :: SoundBufferRecorder -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfSoundBufferRecorder_getSampleRate(const sfSoundBufferRecorder* soundBufferRecorder);


-- | Get the sound buffer containing the captured audio data.
--
-- The sound buffer is valid only after the capture has ended.
-- This function provides a read-only access to the internal
-- sound buffer, but it can be copied if you need to
-- make any modification to it.
getRecorderBuffer
    :: SoundBufferRecorder
    -> IO SoundBuffer -- ^ Read-only access to the sound buffer

getRecorderBuffer = sfSoundBufferRecorder_getBuffer

foreign import ccall unsafe "sfSoundBufferRecorder_getBuffer"
    sfSoundBufferRecorder_getBuffer :: SoundBufferRecorder -> IO SoundBuffer

-- CSFML_AUDIO_API const sfSoundBuffer* sfSoundBufferRecorder_getBuffer(const sfSoundBufferRecorder* soundBufferRecorder);

