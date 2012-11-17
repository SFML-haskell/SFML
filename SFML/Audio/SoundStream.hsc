module SFML.Audio.SoundStream
(
    SoundStreamChunk(..)
,   SoundStreamGetDataCallback
,   SoundStreamSeekCallback
,   createSoundStream
,   destroy
,   play
,   pause
,   stop
,   getAttenuation
,   getLoop
,   getMinDistance
,   getPitch
,   getPlayingOffset
,   getPosition
,   getStatus
,   getVolume
,   isRelativeToListener
,   setAttenuation
,   setLoop
,   setMinDistance
,   setPitch
,   setPlayingOffset
,   setPosition
,   setRelativeToListener
,   setVolume
)
where


import SFML.Audio.SFSound
import SFML.Audio.SoundStatus
import SFML.Audio.Types
import SFML.SFResource
import SFML.System.Time
import SFML.System.Vector3

import Control.Monad ((>=>))
import Data.Word (Word16)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable


#include <SFML/Audio/SoundStream.h>


-- | Defines the data to fill by the OnGetData callback.
data SoundStreamChunk = SoundStreamChunk
    { samples :: Ptr Word16 -- ^ Pointer to the audio samples
    , sampleCount :: Int -- ^ Number of samples pointed by Samples
    }


{-
typedef struct
{
    sfInt16*     samples;     --< Pointer to the audio samples
    unsigned int sampleCount; --< Number of samples pointed by Samples
} sfSoundStreamChunk;
-}


size_SoundStreamChunk = #{size sfSoundStreamChunk}


instance Storable SoundStreamChunk where
    sizeOf _ = size_SoundStreamChunk
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = do
        samples <- #{peek sfSoundStreamChunk, samples} ptr
        sampleCount <- #{peek sfSoundStreamChunk, sampleCount} ptr :: IO CUInt
        return $ SoundStreamChunk samples (fromIntegral sampleCount)
    
    poke ptr (SoundStreamChunk samples sampleCount) = do
        #{poke sfSoundStreamChunk, samples} ptr samples
        #{poke sfSoundStreamChunk, sampleCount} ptr (fromIntegral sampleCount :: CUInt)


-- | Type of the callback used to get a sound stream data.
type SoundStreamGetDataCallback a = Ptr SoundStreamChunk -> Ptr a -> IO CInt


-- | Type of the callback used to seek in a sound stream.
type SoundStreamSeekCallback a = Time -> Ptr a -> IO ()

{-
typedef sfBool (*sfSoundStreamGetDataCallback)(sfSoundStreamChunk*, void*); --< Type of the callback used to get a sound stream data

typedef void   (*sfSoundStreamSeekCallback)(sfTime, void*);                 --< Type of the callback used to seek in a sound stream
-}



-- | Create a new sound stream.
createSoundStream
    :: Ptr (SoundStreamGetDataCallback a) -- ^ Function called when the stream needs more data (can't be NULL)
    -> Ptr (SoundStreamSeekCallback a) -- ^ Function called when the stream seeks (can't be NULL)
    -> Int -- ^ Number of channels to use (1 = mono, 2 = stereo)
    -> Int -- ^ Sample rate of the sound (44100 = CD quality)
    -> Ptr a -- ^ Data to pass to the callback functions
    -> IO SoundStream

createSoundStream = sfSoundStream_create

foreign import ccall unsafe "sfSoundStream_create"
    sfSoundStream_create
        :: Ptr (SoundStreamGetDataCallback a)
        -> Ptr (SoundStreamSeekCallback a)
        -> Int
        -> Int
        -> Ptr a
        -> IO SoundStream

-- CSFML_AUDIO_API sfSoundStream* sfSoundStream_create(sfSoundStreamGetDataCallback onGetData, sfSoundStreamSeekCallback    onSeek, unsigned int channelCount, unsigned int sampleRate, void* userData);


instance SFResource SoundStream where
    
    {-# INLINABLE destroy #-}
    destroy = sfSoundStream_destroy

foreign import ccall unsafe "sfSoundStream_destroy"
    sfSoundStream_destroy :: SoundStream -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_destroy(sfSoundStream* soundStream);


instance SFSound SoundStream where
    
    {-# INLINABLE play #-}
    play = sfSoundStream_play
    
    {-# INLINABLE pause #-}
    pause = sfSoundStream_pause
    
    {-# INLINABLE stop #-}
    stop = sfSoundStream_stop
    
    {-# INLINABLE getAttenuation #-}
    getAttenuation = sfSoundStream_getAttenuation >=> return . realToFrac
    
    {-# INLINABLE getLoop #-}
    getLoop music = fmap (toEnum . fromIntegral) $ sfSoundStream_getLoop music
    
    {-# INLINABLE getMinDistance #-}
    getMinDistance = sfSoundStream_getMinDistance >=> return . realToFrac
    
    {-# INLINABLE getPitch #-}
    getPitch = sfSoundStream_getPitch >=> return . realToFrac
    
    {-# INLINABLE getPlayingOffset #-}
    getPlayingOffset music = alloca $ \ptr -> sfSoundStream_getPlayingOffset_helper music ptr >> peek ptr
    
    {-# INLINABLE getPosition #-}
    getPosition music = alloca $ \ptr -> sfSoundStream_getPosition_helper music ptr >> peek ptr
    
    {-# INLINABLE getStatus #-}
    getStatus = sfSoundStream_getStatus >=> return . toEnum . fromIntegral
    
    {-# INLINABLE getVolume #-}
    getVolume = sfSoundStream_getVolume >=> return . realToFrac
    
    {-# INLINABLE isRelativeToListener #-}
    isRelativeToListener = sfSoundStream_isRelativeToListener >=> return . toEnum . fromIntegral
    
    {-# INLINABLE setAttenuation #-}
    setAttenuation m a = sfSoundStream_setAttenuation m (realToFrac a)
    
    {-# INLINABLE setLoop #-}
    setLoop music val = sfSoundStream_setLoop music (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setMinDistance #-}
    setMinDistance m d = sfSoundStream_setMinDistance m (realToFrac d)
    
    {-# INLINABLE setPitch #-}
    setPitch m p = sfSoundStream_setPitch m (realToFrac p)
    
    {-# INLINABLE setPlayingOffset #-}
    setPlayingOffset = sfSoundStream_setPlayingOffset
    
    {-# INLINABLE setPosition #-}
    setPosition music pos = with pos $ sfSoundStream_setPosition_helper music
    
    {-# INLINABLE setRelativeToListener #-}
    setRelativeToListener music val = sfSoundStream_setRelativeToListener music (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setVolume #-}
    setVolume m v = sfSoundStream_setVolume m (realToFrac v)


foreign import ccall unsafe "sfSoundStream_play"
    sfSoundStream_play :: SoundStream -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_play(sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_pause"
    sfSoundStream_pause :: SoundStream -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_pause(sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_stop"
    sfSoundStream_stop :: SoundStream -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_stop(sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_getStatus"
    sfSoundStream_getStatus :: SoundStream -> IO CInt

-- CSFML_AUDIO_API sfSoundStatus sfSoundStream_getStatus(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_getChannelCount"
    sfSoundStream_getChannelCount :: SoundStream -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfSoundStream_getChannelCount(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_getSampleRate"
    sfSoundStream_getSampleRate :: SoundStream -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfSoundStream_getSampleRate(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_setPitch"
    sfSoundStream_setPitch :: SoundStream -> CFloat -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_setPitch(sfSoundStream* soundStream, float pitch);

foreign import ccall unsafe "sfSoundStream_setVolume"
    sfSoundStream_setVolume :: SoundStream -> CFloat -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_setVolume(sfSoundStream* soundStream, float volume);

foreign import ccall unsafe "sfSoundStream_setPosition_helper"
    sfSoundStream_setPosition_helper :: SoundStream -> Ptr Vec3f -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_setPosition(sfSoundStream* soundStream, sfVector3f position);

foreign import ccall unsafe "sfSoundStream_setRelativeToListener"
    sfSoundStream_setRelativeToListener :: SoundStream -> CInt -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_setRelativeToListener(sfSoundStream* soundStream, sfBool relative);

foreign import ccall unsafe "sfSoundStream_setMinDistance"
    sfSoundStream_setMinDistance :: SoundStream -> CFloat -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_setMinDistance(sfSoundStream* soundStream, float distance);

foreign import ccall unsafe "sfSoundStream_setAttenuation"
    sfSoundStream_setAttenuation :: SoundStream -> CFloat -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_setAttenuation(sfSoundStream* soundStream, float attenuation);

foreign import ccall unsafe "sfSoundStream_setPlayingOffset"
    sfSoundStream_setPlayingOffset :: SoundStream -> Time -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_setPlayingOffset(sfSoundStream* soundStream, sfTime timeOffset);

foreign import ccall unsafe "sfSoundStream_setLoop"
    sfSoundStream_setLoop :: SoundStream -> CInt -> IO ()

-- CSFML_AUDIO_API void sfSoundStream_setLoop(sfSoundStream* soundStream, sfBool loop);

foreign import ccall unsafe "sfSoundStream_getPitch"
    sfSoundStream_getPitch :: SoundStream -> IO CFloat

-- CSFML_AUDIO_API float sfSoundStream_getPitch(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_getVolume"
    sfSoundStream_getVolume :: SoundStream -> IO CFloat

-- CSFML_AUDIO_API float sfSoundStream_getVolume(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_getPosition_helper"
    sfSoundStream_getPosition_helper :: SoundStream -> Ptr Vec3f -> IO ()

-- CSFML_AUDIO_API sfVector3f sfSoundStream_getPosition(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_isRelativeToListener"
    sfSoundStream_isRelativeToListener :: SoundStream -> IO CInt

-- CSFML_AUDIO_API sfBool sfSoundStream_isRelativeToListener(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_getMinDistance"
    sfSoundStream_getMinDistance :: SoundStream -> IO CFloat

-- CSFML_AUDIO_API float sfSoundStream_getMinDistance(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_getAttenuation"
    sfSoundStream_getAttenuation :: SoundStream -> IO CFloat

-- CSFML_AUDIO_API float sfSoundStream_getAttenuation(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_getLoop"
    sfSoundStream_getLoop :: SoundStream -> IO CInt

-- CSFML_AUDIO_API sfBool sfSoundStream_getLoop(const sfSoundStream* soundStream);

foreign import ccall unsafe "sfSoundStream_getPlayingOffset_helper"
    sfSoundStream_getPlayingOffset_helper :: SoundStream -> Ptr Time -> IO ()

-- CSFML_AUDIO_API sfTime sfSoundStream_getPlayingOffset(const sfSoundStream* soundStream);

