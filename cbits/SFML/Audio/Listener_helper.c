#include <SFML/Audio/Listener.h>


void sfListener_setPosition_helper (sfVector3f* position)
{
    sfListener_setPosition (*position);
}


void sfListener_getPosition_helper (sfVector3f* position)
{
    *position = sfListener_getPosition ();
}


void sfListener_setDirection_helper (sfVector3f* orientation)
{
    sfListener_setDirection (*orientation);
}


void sfListener_getDirection_helper (sfVector3f* direction)
{
    *direction = sfListener_getDirection ();
}

