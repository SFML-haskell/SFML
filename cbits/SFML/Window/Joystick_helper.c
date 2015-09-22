#include <SFML/Window/Joystick.h>


void sfJoystick_getIdentification_helper (unsigned int joystick, sfJoystickIdentification* ident)
{
    *ident = sfJoystick_getIdentification(joystick);
}
