#include <SFML/Audio/Music.h>


void sfMusic_getDuration_helper (const sfMusic* music, sfTime* time)
{
    *time = sfMusic_getDuration (music);
}


void sfMusic_getPlayingOffset_helper (const sfMusic* music, sfTime* time)
{
    *time = sfMusic_getPlayingOffset (music);
}


void sfMusic_setPosition_helper (sfMusic* music, sfVector3f* position)
{
    sfMusic_setPosition (music, *position);
}


void sfMusic_getPosition_helper (const sfMusic* music, sfVector3f* position)
{
    *position = sfMusic_getPosition (music);
}

