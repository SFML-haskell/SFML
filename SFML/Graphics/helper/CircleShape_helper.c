#include <SFML/Graphics/CircleShape.h>


void sfCircleShape_setPosition_helper (sfCircleShape* shape, sfVector2f* position)
{
    sfCircleShape_setPosition (shape, *position);
}


void sfCircleShape_setScale_helper (sfCircleShape* shape, sfVector2f* scale)
{
    sfCircleShape_setScale (shape, *scale);
}


void sfCircleShape_setOrigin_helper (sfCircleShape* shape, sfVector2f* origin)
{
    sfCircleShape_setOrigin (shape, *origin);
}


void sfCircleShape_getPosition_helper (const sfCircleShape* shape, sfVector2f* position)
{
    *position = sfCircleShape_getPosition (shape);
}


void sfCircleShape_getScale_helper (const sfCircleShape* shape, sfVector2f* scale)
{
    *scale = sfCircleShape_getScale (shape);
}


void sfCircleShape_getOrigin_helper (const sfCircleShape* shape, sfVector2f* origin)
{
    *origin = sfCircleShape_getOrigin (shape);
}


void sfCircleShape_move_helper (sfCircleShape* shape, sfVector2f* offset)
{
    sfCircleShape_move (shape, *offset);
}


void sfCircleShape_scale_helper (sfCircleShape* shape, sfVector2f* factors)
{
    sfCircleShape_scale (shape, *factors);
}


void sfCircleShape_getTransform_helper (const sfCircleShape* shape, sfTransform* transform)
{
    *transform = sfCircleShape_getTransform (shape);
}


void sfCircleShape_getInverseTransform_helper (const sfCircleShape* shape, sfTransform* itransform)
{
    *itransform = sfCircleShape_getInverseTransform (shape);
}


void sfCircleShape_setTextureRect_helper (sfCircleShape* shape, sfIntRect* rect)
{
    sfCircleShape_setTextureRect (shape, *rect);
}


void sfCircleShape_getTextureRect_helper (const sfCircleShape* shape, sfIntRect* rect)
{
    *rect = sfCircleShape_getTextureRect (shape);
}


void sfCircleShape_setFillColor_helper (sfCircleShape* shape, sfColor* color)
{
    sfCircleShape_setFillColor (shape, *color);
}


void sfCircleShape_setOutlineColor_helper (sfCircleShape* shape, sfColor* color)
{
    sfCircleShape_setOutlineColor (shape, *color);
}


void sfCircleShape_getFillColor_helper (const sfCircleShape* shape, sfColor* color)
{
    *color = sfCircleShape_getFillColor (shape);
}


void sfCircleShape_getOutlineColor_helper (const sfCircleShape* shape, sfColor* color)
{
    *color = sfCircleShape_getOutlineColor (shape);
}


void sfCircleShape_getPoint_helper (const sfCircleShape* shape, unsigned int index, sfVector2f* point)
{
    *point = sfCircleShape_getPoint (shape, index);
}


void sfCircleShape_getLocalBounds_helper (const sfCircleShape* shape, sfFloatRect* rect)
{
    *rect = sfCircleShape_getLocalBounds (shape);
}


void sfCircleShape_getGlobalBounds_helper (const sfCircleShape* shape, sfFloatRect* rect)
{
    *rect = sfCircleShape_getGlobalBounds (shape);
}

