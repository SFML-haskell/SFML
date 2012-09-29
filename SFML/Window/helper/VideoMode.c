#include <SFML/Window/VideoMode.h>


sfBool sfVideoMode_isValid_helper (sfVideoMode* mode)
{
    return sfVideoMode_isValid (*mode);
}


void sfVideoMode_getDesktopMode_helper (sfVideoMode* mode)
{
    *mode = sfVideoMode_getDesktopMode ();
}

