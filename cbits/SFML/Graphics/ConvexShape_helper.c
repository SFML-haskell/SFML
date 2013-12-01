#include <SFML/Graphics/ConvexShape.h>


void sfConvexShape_setPosition_helper (sfConvexShape* shape, sfVector2f* position)
{
    sfConvexShape_setPosition (shape, *position);
}


void sfConvexShape_setScale_helper (sfConvexShape* shape, sfVector2f* scale)
{
    sfConvexShape_setScale (shape, *scale);
}


void sfConvexShape_setOrigin_helper (sfConvexShape* shape, sfVector2f* origin)
{
    sfConvexShape_setOrigin (shape, *origin);
}


void sfConvexShape_getPosition_helper (const sfConvexShape* shape, sfVector2f* position)
{
    *position = sfConvexShape_getPosition (shape);
}


void sfConvexShape_getScale_helper (const sfConvexShape* shape, sfVector2f* scale)
{
    *scale = sfConvexShape_getScale (shape);
}


void sfConvexShape_getOrigin_helper (const sfConvexShape* shape, sfVector2f* origin)
{
    *origin = sfConvexShape_getOrigin (shape);
}


void sfConvexShape_move_helper (sfConvexShape* shape, sfVector2f* offset)
{
    sfConvexShape_move (shape, *offset);
}


void sfConvexShape_scale_helper (sfConvexShape* shape, sfVector2f* factors)
{
    sfConvexShape_scale (shape, *factors);
}


void sfConvexShape_getTransform_helper (const sfConvexShape* shape, sfTransform* transform)
{
    *transform = sfConvexShape_getTransform (shape);
}


void sfConvexShape_getInverseTransform_helper (const sfConvexShape* shape, sfTransform* itransform)
{
    *itransform = sfConvexShape_getInverseTransform (shape);
}


void sfConvexShape_setTextureRect_helper (sfConvexShape* shape, sfIntRect* rect)
{
    sfConvexShape_setTextureRect (shape, *rect);
}


void sfConvexShape_getTextureRect_helper (const sfConvexShape* shape, sfIntRect* rect)
{
    *rect = sfConvexShape_getTextureRect (shape);
}


void sfConvexShape_setFillColor_helper (sfConvexShape* shape, sfColor* color)
{
    sfConvexShape_setFillColor (shape, *color);
}


void sfConvexShape_setOutlineColor_helper (sfConvexShape* shape, sfColor* color)
{
    sfConvexShape_setOutlineColor (shape, *color);
}


void sfConvexShape_getFillColor_helper (const sfConvexShape* shape, sfColor* color)
{
    *color = sfConvexShape_getFillColor (shape);
}


void sfConvexShape_getOutlineColor_helper (const sfConvexShape* shape, sfColor* color)
{
    *color = sfConvexShape_getOutlineColor (shape);
}


void sfConvexShape_getPoint_helper (const sfConvexShape* shape, unsigned int index, sfVector2f* point)
{
    *point = sfConvexShape_getPoint (shape, index);
}


void sfConvexShape_getLocalBounds_helper (const sfConvexShape* shape, sfFloatRect* rect)
{
    *rect = sfConvexShape_getLocalBounds (shape);
}


void sfConvexShape_getGlobalBounds_helper (const sfConvexShape* shape, sfFloatRect* rect)
{
    *rect = sfConvexShape_getGlobalBounds (shape);
}


void sfConvexShape_setPoint_helper (sfConvexShape* shape, unsigned int index, sfVector2f* point)
{
    sfConvexShape_setPoint (shape, index, *point);
}

