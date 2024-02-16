#include <SFML/Graphics/Glsl.h>
#include <SFML/Graphics/Shader.h>

void sfShader_setVector2Uniform_helper(sfShader *shader, const char *name,
                                       sfVector2f *vector) {
  sfShader_setVec2Uniform(shader, name, *vector);
}

void sfShader_setVector3Uniform_helper(sfShader *shader, const char *name,
                                       sfVector3f *vector) {
  sfShader_setVec3Uniform(shader, name, *vector);
}

void sfShader_setColorUniform_helper(sfShader *shader, const char *name,
                                     sfColor *color) {
  sfShader_setColorUniform(shader, name, *color);
}

void sfShader_setTransformUniform_helper(sfShader *shader, const char *name,
                                         sfTransform *transform) {
  sfShader_setMat4Uniform(shader, name, (sfGlslMat4 *)transform);
}
