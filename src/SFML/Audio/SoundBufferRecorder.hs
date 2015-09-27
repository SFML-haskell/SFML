{-# LANGUAGE DeriveDataTypeable #-}
module SFML.Audio.SoundBufferRecorder
(
    module SFML.Utils
,   createSoundBufferRecorder
,   destroy
,   startRecording
,   stopRecording
,   getSampleRate
,   getRecorderBuffer
)
where


import SFML.Audio.SFSampled
import SFML.Audio.SFSoundRecorder
import SFML.Audio.Types
import SFML.SFException
import SFML.SFResource
import SFML.Utils

import Control.Exception
import Control.Monad ((>=>))
import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr (nullPtr)


checkNull :: SoundBufferRecorder -> Maybe SoundBufferRecorder
checkNull sbr@(SoundBufferRecorder ptr) = if ptr == nullPtr then Nothing else Just sbr


-- | Create a new sound buffer recorder.
createSoundBufferRecorder :: IO (Either SFException SoundBufferRecorder)
createSoundBufferRecorder =
    let err = SFException $ "Failed creating sound buffer recorder"
    in fmap (tagErr err . checkNull) sfSoundBufferRecorder_create

foreign import ccall unsafe "sfSoundBufferRecorder_create"
    sfSoundBufferRecorder_create :: IO SoundBufferRecorder

-- \return A new sfSoundBufferRecorder object (NULL if failed)

-- CSFML_AUDIO_API sfSoundBufferRecorder* sfSoundBufferRecorder_create(void);


instance SFResource SoundBufferRecorder where

    {-# INLINABLE destroy #-}
    destroy = sfSoundBufferRecorder_destroy

foreign import ccall unsafe "sfSoundBufferRecorder_destroy"
    sfSoundBufferRecorder_destroy :: SoundBufferRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundBufferRecorder_destroy(sfSoundBufferRecorder* soundBufferRecorder);


instance SFSoundRecorder SoundBufferRecorder where

    {-# INLINABLE startRecording #-}
    startRecording rec rate = ((/=0) . fromIntegral) <$> sfSoundBufferRecorder_start rec (fromIntegral rate)

    {-# INLINABLE stopRecording #-}
    stopRecording = sfSoundBufferRecorder_stop


foreign import ccall unsafe "sfSoundBufferRecorder_start"
    sfSoundBufferRecorder_start :: SoundBufferRecorder -> CUInt -> IO CInt

-- CSFML_AUDIO_API void sfSoundBufferRecorder_start(sfSoundBufferRecorder* soundBufferRecorder, unsigned int sampleRate);

foreign import ccall unsafe "sfSoundBufferRecorder_stop"
    sfSoundBufferRecorder_stop :: SoundBufferRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundBufferRecorder_stop(sfSoundBufferRecorder* soundBufferRecorder);


instance SFSampled SoundBufferRecorder where

    {-# INLINABLE getSampleRate #-}
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

