#include <SFML/Window/Mouse.h>


void sfMouse_getPosition_helper (const sfWindow* relativeTo, sfVector2i* pos)
{
    *pos = sfMouse_getPosition (relativeTo);
}


void sfMouse_setPosition_helper (sfVector2i* position, const sfWindow* relativeTo)
{
    sfMouse_setPosition (*position, relativeTo);
}

