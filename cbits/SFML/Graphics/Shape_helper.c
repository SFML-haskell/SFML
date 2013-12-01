#include <SFML/Graphics/Shape.h>


void sfShape_setPosition_helper (sfShape* shape, sfVector2f* position)
{
    sfShape_setPosition (shape, *position);
}


void sfShape_setScale_helper (sfShape* shape, sfVector2f* scale)
{
    sfShape_setScale (shape, *scale);
}


void sfShape_setOrigin_helper (sfShape* shape, sfVector2f* origin)
{
    sfShape_setOrigin (shape, *origin);
}


void sfShape_getPosition_helper (const sfShape* shape, sfVector2f* position)
{
    *position = sfShape_getPosition (shape);
}


void sfShape_getScale_helper (const sfShape* shape, sfVector2f* scale)
{
    *scale = sfShape_getScale (shape);
}


void sfShape_getOrigin_helper (const sfShape* shape, sfVector2f* origin)
{
    *origin = sfShape_getOrigin (shape);
}


void sfShape_move_helper (sfShape* shape, sfVector2f* offset)
{
    sfShape_move (shape, *offset);
}


void sfShape_scale_helper (sfShape* shape, sfVector2f* factors)
{
    sfShape_scale (shape, *factors);
}


void sfShape_getTransform_helper (const sfShape* shape, sfTransform* transform)
{
    *transform = sfShape_getTransform (shape);
}


void sfShape_getInverseTransform_helper (const sfShape* shape, sfTransform* itransform)
{
    *itransform = sfShape_getInverseTransform (shape);
}


void sfShape_setTextureRect_helper (sfShape* shape, sfIntRect* rect)
{
    sfShape_setTextureRect (shape, *rect);
}


void sfShape_getTextureRect_helper (const sfShape* shape, sfIntRect* rect)
{
    *rect = sfShape_getTextureRect (shape);
}


void sfShape_setFillColor_helper (sfShape* shape, sfColor* color)
{
    sfShape_setFillColor (shape, *color);
}


void sfShape_setOutlineColor_helper (sfShape* shape, sfColor* color)
{
    sfShape_setOutlineColor (shape, *color);
}


void sfShape_getFillColor_helper (const sfShape* shape, sfColor* color)
{
    *color = sfShape_getFillColor (shape);
}


void sfShape_getOutlineColor_helper (const sfShape* shape, sfColor* color)
{
    *color = sfShape_getOutlineColor (shape);
}


void sfShape_getPoint_helper (const sfShape* shape, unsigned int index, sfVector2f* point)
{
    *point = sfShape_getPoint (shape, index);
}


void sfShape_getLocalBounds_helper (const sfShape* shape, sfFloatRect* rect)
{
    *rect = sfShape_getLocalBounds (shape);
}


void sfShape_getGlobalBounds_helper (const sfShape* shape, sfFloatRect* rect)
{
    *rect = sfShape_getGlobalBounds (shape);
}

