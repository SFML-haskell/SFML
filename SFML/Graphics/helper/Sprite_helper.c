#include <SFML/Graphics/Sprite.h>


void sfSprite_setPosition_helper (sfSprite* sprite, sfVector2f* position)
{
    sfSprite_setPosition (sprite, *position);
}


void sfSprite_setScale_helper (sfSprite* sprite, sfVector2f* scale)
{
    sfSprite_setScale (sprite, *scale);
}


void sfSprite_setOrigin_helper (sfSprite* sprite, sfVector2f* origin)
{
    sfSprite_setOrigin (sprite, *origin);
}


void sfSprite_getPosition_helper (const sfSprite* sprite, sfVector2f* position)
{
    *position = sfSprite_getPosition (sprite);
}


void sfSprite_getScale_helper (const sfSprite* sprite, sfVector2f* scale)
{
    *scale = sfSprite_getScale (sprite);
}


void sfSprite_getOrigin_helper (const sfSprite* sprite, sfVector2f* origin)
{
    *origin = sfSprite_getOrigin (sprite);
}


void sfSprite_move_helper (sfSprite* sprite, sfVector2f* offset)
{
    sfSprite_move (sprite, *offset);
}


void sfSprite_scale_helper (sfSprite* sprite, sfVector2f* factors)
{
    sfSprite_scale (sprite, *factors);
}


void sfSprite_getTransform_helper (const sfSprite* sprite, sfTransform* transform)
{
    *transform = sfSprite_getTransform (sprite);
}


void sfSprite_getInverseTransform_helper (const sfSprite* sprite, sfTransform* transform)
{
    *transform = sfSprite_getInverseTransform (sprite);
}

