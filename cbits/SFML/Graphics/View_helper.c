#include <SFML/Graphics/View.h>


sfView* sfView_createFromRect_helper (sfFloatRect* rectangle)
{
    return sfView_createFromRect (*rectangle);
}


void sfView_setCenter_helper (sfView* view, sfVector2f* center)
{
    sfView_setCenter (view, *center);
}


void sfView_setSize_helper (sfView* view, sfVector2f* size)
{
    sfView_setSize (view, *size);
}


void sfView_setViewport_helper (sfView* view, sfFloatRect* viewport)
{
    sfView_setViewport (view, *viewport);
}


void sfView_reset_helper (sfView* view, sfFloatRect* rectangle)
{
    sfView_reset (view, *rectangle);
}


void sfView_getSize_helper (const sfView* view, sfVector2f* size)
{
    *size = sfView_getSize (view);
}


void sfView_getViewport_helper (const sfView* view, sfFloatRect* vp)
{
    *vp = sfView_getViewport (view);
}


void sfView_move_helper (sfView* view, sfVector2f* offset)
{
    sfView_move (view, *offset);
}

