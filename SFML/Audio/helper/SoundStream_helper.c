#include <SFML/Audio/SoundStream.h>


void sfSoundStream_getPlayingOffset_helper (const sfSoundStream* stream, sfTime* time)
{
    *time = sfSoundStream_getPlayingOffset (stream);
}


void sfSoundStream_setPosition_helper (sfSoundStream* stream, sfVector3f* position)
{
    sfSoundStream_setPosition (stream, *position);
}


void sfSoundStream_getPosition_helper (const sfSoundStream* stream, sfVector3f* position)
{
    *position = sfSoundStream_getPosition (stream);
}

