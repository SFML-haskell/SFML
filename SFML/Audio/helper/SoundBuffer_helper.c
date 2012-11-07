#include <SFML/Audio/SoundBuffer.h>


void sfSoundBuffer_getDuration_helper (const sfSoundBuffer* soundBuffer, sfTime* time)
{
    *time = sfSoundBuffer_getDuration (soundBuffer);
}

