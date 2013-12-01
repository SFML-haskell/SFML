#include <SFML/System/Clock.h>


void sfClock_getElapsedTime_helper (const sfClock* clock, sfTime* time)
{
    *time = sfClock_getElapsedTime (clock);
}


void sfClock_restart_helper (sfClock* clock, sfTime* time)
{
    *time = sfClock_restart (clock);
}

