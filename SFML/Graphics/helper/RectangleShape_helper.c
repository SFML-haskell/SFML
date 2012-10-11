#include <SFML/Graphics/RectangleShape.h>


void sfRectangleShape_setPosition_helper (sfRectangleShape* shape, sfVector2f* position)
{
    sfRectangleShape_setPosition (shape, *position);
}


void sfRectangleShape_setScale_helper (sfRectangleShape* shape, sfVector2f* scale)
{
    sfRectangleShape_setScale (shape, *scale);
}


void sfRectangleShape_setOrigin_helper (sfRectangleShape* shape, sfVector2f* origin)
{
    sfRectangleShape_setOrigin (shape, *origin);
}


void sfRectangleShape_getPosition_helper (const sfRectangleShape* shape, sfVector2f* position)
{
    *position = sfRectangleShape_getPosition (shape);
}


void sfRectangleShape_getScale_helper (const sfRectangleShape* shape, sfVector2f* scale)
{
    *scale = sfRectangleShape_getScale (shape);
}


void sfRectangleShape_getOrigin_helper (const sfRectangleShape* shape, sfVector2f* origin)
{
    *origin = sfRectangleShape_getOrigin (shape);
}


void sfRectangleShape_move_helper (sfRectangleShape* shape, sfVector2f* offset)
{
    sfRectangleShape_move (shape, *offset);
}


void sfRectangleShape_scale_helper (sfRectangleShape* shape, sfVector2f* factors)
{
    sfRectangleShape_scale (shape, *factors);
}


void sfRectangleShape_getTransform_helper (const sfRectangleShape* shape, sfTransform* transform)
{
    *transform = sfRectangleShape_getTransform (shape);
}


void sfRectangleShape_getInverseTransform_helper (const sfRectangleShape* shape, sfTransform* itransform)
{
    *itransform = sfRectangleShape_getInverseTransform (shape);
}


void sfRectangleShape_setTextureRect_helper (sfRectangleShape* shape, sfIntRect* rect)
{
    sfRectangleShape_setTextureRect (shape, *rect);
}


void sfRectangleShape_getTextureRect_helper (const sfRectangleShape* shape, sfIntRect* rect)
{
    *rect = sfRectangleShape_getTextureRect (shape);
}


void sfRectangleShape_setFillColor_helper (sfRectangleShape* shape, sfColor* color)
{
    sfRectangleShape_setFillColor (shape, *color);
}


void sfRectangleShape_setOutlineColor_helper (sfRectangleShape* shape, sfColor* color)
{
    sfRectangleShape_setOutlineColor (shape, *color);
}


void sfRectangleShape_getFillColor_helper (const sfRectangleShape* shape, sfColor* color)
{
    *color = sfRectangleShape_getFillColor (shape);
}


void sfRectangleShape_getOutlineColor_helper (const sfRectangleShape* shape, sfColor* color)
{
    *color = sfRectangleShape_getOutlineColor (shape);
}


void sfRectangleShape_getPoint_helper (const sfRectangleShape* shape, unsigned int index, sfVector2f* point)
{
    *point = sfRectangleShape_getPoint (shape, index);
}


void sfRectangleShape_getLocalBounds_helper (const sfRectangleShape* shape, sfFloatRect* rect)
{
    *rect = sfRectangleShape_getLocalBounds (shape);
}


void sfRectangleShape_getGlobalBounds_helper (const sfRectangleShape* shape, sfFloatRect* rect)
{
    *rect = sfRectangleShape_getGlobalBounds (shape);
}


void sfRectangleShape_setSize_helper (sfRectangleShape* shape, sfVector2f* size)
{
    sfRectangleShape_setSize (shape, *size);
}


void sfRectangleShape_getSize_helper (const sfRectangleShape* shape, sfVector2f* size)
{
    *size = sfRectangleShape_getSize (shape);
}

