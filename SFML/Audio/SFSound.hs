module SFML.Audio.SFSound
where


import SFML.Audio.SoundStatus
import SFML.System.Time
import SFML.System.Vector3


class SFSound a where
    
    -- | Start or resume playing a sound.
    -- 
    -- This function starts the sound if it was stopped, resumes
    -- it if it was paused, and restarts it from beginning if it
    -- was it already playing.
    -- 
    -- This function uses its own thread so that it doesn't block
    -- the rest of the program while the sound is played.
    play :: a -> IO ()
    
    -- | Pause a sound.
    -- 
    -- This function pauses the sound if it was playing,
    -- otherwise (sound already paused or stopped) it has no effect.
    pause :: a -> IO ()
    
    -- | Stop playing a sound.
    -- 
    -- This function stops the sound if it was playing or paused,
    -- and does nothing if it was already stopped.
    -- 
    -- It also resets the playing position (unlike 'pause').
    stop :: a -> IO ()
    
    -- | Get the attenuation factor of a sound.
    getAttenuation :: a -> IO Float
    
    -- | Tell whether or not a sound is in loop mode.
    getLoop :: a -> IO Bool
    
    -- | Get the minimum distance of a sound.
    getMinDistance :: a -> IO Float
    
    -- | Get the pitch of a sound.
    getPitch :: a -> IO Float
    
    -- | Get the current playing position of a sound.
    getPlayingOffset :: a -> IO Time
    
    -- | Get the 3D position of a sound in the audio scene.
    getPosition :: a -> IO Vec3f
    
    -- | Get the current status of a sound (stopped, paused, playing).
    getStatus :: a -> IO SoundStatus
    
    -- | Get the volume of a sound.
    getVolume :: a -> IO Float
    
    -- | Tell whether a sound's position is relative to the listener or is absolute.
    isRelativeToListener :: a -> IO Bool
    
    -- | Set the attenuation factor of a sound.
    -- 
    -- The attenuation is a multiplicative factor which makes
    -- the sound more or less loud according to its distance
    -- from the listener. An attenuation of 0 will produce a
    -- non-attenuated sound, i.e. its volume will always be the same
    -- whether it is heard from near or from far. On the other hand,
    -- an attenuation value such as 100 will make the sound fade out
    -- very quickly as it gets further from the listener.
    -- 
    -- The default value of the attenuation is 1.
    setAttenuation :: a -> Float -> IO ()
    
    -- | Set whether or not a sound should loop after reaching the end.
    -- 
    -- If set, the sound will restart from beginning after
    -- reaching the end and so on, until it is stopped or
    -- 'setLoop' 'False' is called.
    -- 
    -- The default looping state for sounds is false.
    setLoop :: a -> Bool -> IO ()
    
    -- | Set the minimum distance of a sound.
    -- 
    -- The minimum distance of a sound is the maximum
    -- distance at which it is heard at its maximum volume. Further
    -- than the minimum distance, it will start to fade out according
    -- to its attenuation factor. A value of 0 (inside the head
    -- of the listener) is an invalid value and is forbidden.
    -- 
    -- The default value of the minimum distance is 1.
    setMinDistance :: a -> Float -> IO ()
    
    -- | Set the pitch of a sound.
    -- 
    -- The pitch represents the perceived fundamental frequency
    -- of a sound; thus you can make a sound more acute or grave
    -- by changing its pitch. A side effect of changing the pitch
    -- is to modify the playing speed of the sound as well.
    -- 
    -- The default value for the pitch is 1.
    setPitch :: a -> Float -> IO ()
    
    -- | Change the current playing position of a sound.
    -- 
    -- The playing position can be changed when the sound is
    -- either paused or playing.
    setPlayingOffset :: a -> Time -> IO ()
    
    -- | Set the 3D position of a sound in the audio scene.
    -- 
    -- Only sounds with one channel (mono sounds) can be
    -- spatialized.
    -- 
    -- The default position of a sound is (0, 0, 0).
    setPosition :: a -> Vec3f -> IO ()
    
    -- | Make the sound's position relative to the listener or absolute.
    -- 
    -- Making a sound relative to the listener will ensure that it will always
    -- be played the same way regardless the position of the listener.
    -- This can be useful for non-spatialized sounds, sounds that are
    -- produced by the listener, or sounds attached to it.
    -- 
    -- The default value is false (position is absolute).
    setRelativeToListener :: a -> Bool -> IO ()
    
    -- | Set the volume of a sound.
    -- 
    -- The volume is a value between 0 (mute) and 100 (full volume).
    -- 
    -- The default value for the volume is 100.
    setVolume :: a -> Float -> IO ()

