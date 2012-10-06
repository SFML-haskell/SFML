#include <SFML/Graphics/RenderTexture.h>


void sfRenderTexture_getSize_helper (const sfRenderTexture* renderTexture, sfVector2u* size)
{
    *size = sfRenderTexture_getSize (renderTexture);
}


void sfRenderTexture_clear_helper (sfRenderTexture* renderTexture, sfColor* color)
{
    sfRenderTexture_clear (renderTexture, *color);
}


void sfRenderTexture_getViewport_helper (const sfRenderTexture* renderTexture, const sfView* view, sfIntRect* vp)
{
    *vp = sfRenderTexture_getViewport (renderTexture, view);
}


void sfRenderTexture_convertCoords_helper
(const sfRenderTexture* renderTexture, sfVector2i* point, const sfView* targetView, sfVector2f* coords)
{
    *coords = sfRenderTexture_convertCoords (renderTexture, *point, targetView);
}

