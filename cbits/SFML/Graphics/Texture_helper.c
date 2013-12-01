#include <SFML/Graphics/Texture.h>


void sfTexture_getSize_helper (const sfTexture* texture, sfVector2u* size)
{
    *size = sfTexture_getSize (texture);
}

