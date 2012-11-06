#include <SFML/Audio/Sound.h>


void sfSound_setPosition_helper (sfSound* sound, sfVector3f* position)
{
    sfSound_setPosition (sound, *position);
}


void sfSound_getPosition_helper (const sfSound* sound, sfVector3f* position)
{
    *position = sfSound_getPosition (sound);
}


void sfSound_getPlayingOffset_helper (const sfSound* sound, sfTime* time)
{
    *time = sfSound_getPlayingOffset (sound);
}

