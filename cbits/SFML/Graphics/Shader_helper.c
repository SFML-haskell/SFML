#include <SFML/Graphics/Shader.h>


void sfShader_setVector2Parameter_helper (sfShader* shader, const char* name, sfVector2f* vector)
{
    sfShader_setVector2Parameter (shader, name, *vector);
}


void sfShader_setVector3Parameter_helper (sfShader* shader, const char* name, sfVector3f* vector)
{
    sfShader_setVector3Parameter (shader, name, *vector);
}


void sfShader_setColorParameter_helper (sfShader* shader, const char* name, sfColor* color)
{
    sfShader_setColorParameter (shader, name, *color);
}


void sfShader_setTransformParameter_helper (sfShader* shader, const char* name, sfTransform* transform)
{
    sfShader_setTransformParameter (shader, name, *transform);
}

