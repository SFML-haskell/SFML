#include <SFML/Graphics/Text.h>


void sfText_setPosition_helper (sfText* text, sfVector2f* position)
{
    sfText_setPosition (text, *position);
}


void sfText_setScale_helper (sfText* text, sfVector2f* scale)
{
    sfText_setScale (text, *scale);
}


void sfText_setOrigin_helper (sfText* text, sfVector2f* origin)
{
    sfText_setOrigin (text, *origin);
}


void sfText_getPosition_helper (const sfText* text, sfVector2f* position)
{
    *position = sfText_getPosition (text);
}


void sfText_getScale_helper (const sfText* text, sfVector2f* scale)
{
    *scale = sfText_getScale (text);
}


void sfText_getOrigin_helper (const sfText* text, sfVector2f* origin)
{
    *origin = sfText_getOrigin (text);
}


void sfText_move_helper (sfText* text, sfVector2f* offset)
{
    sfText_move (text, *offset);
}


void sfText_scale_helper (sfText* text, sfVector2f* factors)
{
    sfText_scale (text, *factors);
}


void sfText_getTransform_helper (const sfText* text, sfTransform* transform)
{
    *transform = sfText_getTransform (text);
}


void sfText_getInverseTransform_helper (const sfText* text, sfTransform* itransform)
{
    *itransform = sfText_getInverseTransform (text);
}


void sfText_setColor_helper (sfText* text, sfColor* color)
{
    sfText_setColor (text, *color);
}


void sfText_getColor_helper (const sfText* text, sfColor* color)
{
    *color = sfText_getColor (text);
}


void sfText_findCharacterPos_helper (const sfText* text, size_t index, sfVector2f* position)
{
    *position = sfText_findCharacterPos (text, index);
}


void sfText_getLocalBounds_helper (const sfText* text, sfFloatRect* rect)
{
    *rect = sfText_getLocalBounds (text);
}


void sfText_getGlobalBounds_helper (const sfText* text, sfFloatRect* rect)
{
    *rect = sfText_getGlobalBounds (text);
}

