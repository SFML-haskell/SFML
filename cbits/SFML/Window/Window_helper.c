#include <SFML/Window/Mouse.h>
#include <SFML/Window/Window.h>


sfWindow* sfWindow_create_helper
(sfVideoMode* mode, char* title, sfUint32 style, sfContextSettings* settings)
{
    return sfWindow_create (*mode, title, style, settings);
}


void sfWindow_getSettings_helper (sfWindow* window, sfContextSettings* settings)
{
    *settings = sfWindow_getSettings (window);
}


void sfWindow_getPosition_helper (sfWindow* window, sfVector2i* pos)
{
    *pos = sfWindow_getPosition (window);
}


void sfWindow_setPosition_helper (sfWindow* window, sfVector2i* position)
{
    sfWindow_setPosition (window, *position);
}


void sfWindow_getSize_helper (const sfWindow* window, sfVector2u* size)
{
    *size = sfWindow_getSize (window);
}


void sfWindow_setSize_helper (sfWindow* window, sfVector2u* size)
{
    sfWindow_setSize (window, *size);
}


void sfMouse_getPosition_helper (const sfWindow* relativeTo, sfVector2i* pos)
{
    *pos = sfMouse_getPosition (relativeTo);
}


void sfMouse_setPosition_helper (sfVector2i* position, const sfWindow* relativeTo)
{
    sfMouse_setPosition (*position, relativeTo);
}


