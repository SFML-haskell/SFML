#include <SFML/Graphics/RenderWindow.h>


sfRenderWindow* sfRenderWindow_create_helper
(sfVideoMode* mode, const char* title, sfUint32 style, const sfContextSettings* settings)
{
    return sfRenderWindow_create (*mode, title, style, settings);
}


void sfRenderWindow_getSettings_helper (const sfRenderWindow* renderWindow, sfContextSettings* settings)
{
    *settings = sfRenderWindow_getSettings (renderWindow);
}


void sfRenderWindow_getPosition_helper (const sfRenderWindow* renderWindow, sfVector2i* position)
{
    *position = sfRenderWindow_getPosition (renderWindow);
}


void sfRenderWindow_setPosition_helper (sfRenderWindow* renderWindow, sfVector2i* position)
{
    sfRenderWindow_setPosition (renderWindow, *position);
}


void sfRenderWindow_getSize_helper (const sfRenderWindow* renderWindow, sfVector2u* size)
{
    *size = sfRenderWindow_getSize (renderWindow);
}


void sfRenderWindow_setSize_helper (sfRenderWindow* renderWindow, sfVector2u* size)
{
    sfRenderWindow_setSize (renderWindow, *size);
}


void sfRenderWindow_clear_helper (sfRenderWindow* renderWindow, sfColor* color)
{
    sfRenderWindow_clear (renderWindow, *color);
}


void sfRenderWindow_getViewport_helper (const sfRenderWindow* renderWindow, const sfView* view, sfIntRect* rect)
{
    *rect = sfRenderWindow_getViewport (renderWindow, view);
}


void sfRenderWindow_convertCoords_helper
(const sfRenderWindow* renderWindow, sfVector2i point, const sfView* targetView, sfVector2f* out)
{
    *out = sfRenderWindow_convertCoords (renderWindow, point, targetView);
}


void sfMouse_getPositionRenderWindow_helper (const sfRenderWindow* relativeTo, sfVector2i* position)
{
    *position = sfMouse_getPositionRenderWindow (relativeTo);
}


void sfMouse_setPositionRenderWindow_helper (sfVector2i* position, const sfRenderWindow* relativeTo)
{
    sfMouse_setPositionRenderWindow (*position, relativeTo);
}

