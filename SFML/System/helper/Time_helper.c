#include <SFML/System/Time.h>


void sfSeconds_helper (float amount, sfTime* time)
{
    *time = sfSeconds (amount);
}


void sfMilliseconds_helper (sfInt32 amount, sfTime* time)
{
    *time = sfMilliseconds (amount);
}


void sfMicroseconds_helper (sfInt64 amount, sfTime* time)
{
    *time = sfMicroseconds (amount);
}

