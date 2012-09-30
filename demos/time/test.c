#include <SFML/System/Clock.h>

#include <stdio.h>


int main (void)
{
    sfClock* clock = sfClock_create ();
    
    int i = 2;
    while (i >= 0)
    {
        sfTime t = sfClock_getElapsedTime (clock);
        if (sfTime_asSeconds (t) >= 1.0f)
        {
            printf ("tick\n");
            sfClock_restart (clock);
            i--;
        }
    }
    
    sfClock_destroy (clock);
    return 0;
}

