module SFML.Audio.Types
where


import Foreign.Ptr


newtype Music = Music (Ptr Music)
newtype Sound = Sound (Ptr Sound)
newtype SoundBuffer = SoundBuffer (Ptr SoundBuffer)
newtype SoundBufferRecorder = SoundBufferRecorder (Ptr SoundBufferRecorder)
newtype SoundRecorder = SoundRecorder (Ptr SoundRecorder)
newtype SoundStream = SoundStream (Ptr SoundStream)

