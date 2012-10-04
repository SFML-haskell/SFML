#include <SFML/Graphics/Image.h>


sfImage* sfImage_createFromColor_helper (unsigned int width, unsigned int height, sfColor* color)
{
    return sfImage_createFromColor (width, height, *color);
}


void sfImage_getSize_helper (const sfImage* image, sfVector2u* size)
{
    *size = sfImage_getSize (image);
}


void sfImage_createMaskFromColor_helper (sfImage* image, sfColor* color, sfUint8 alpha)
{
    sfImage_createMaskFromColor (image, *color, alpha);
}


void sfImage_copyImage_helper
(sfImage* image, const sfImage* source, unsigned int destX, unsigned int destY, sfIntRect* sourceRect, sfBool applyAlpha)
{
    sfImage_copyImage (image, source, destX, destY, *sourceRect, applyAlpha);
}


void sfImage_setPixel_helper (sfImage* image, unsigned int x, unsigned int y, sfColor* color)
{
    sfImage_setPixel (image, x, y, *color);
}


void sfImage_getPixel_helper (const sfImage* image, unsigned int x, unsigned int y, sfColor* color)
{
    *color = sfImage_getPixel (image, x, y);
}

