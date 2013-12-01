#include <SFML/Graphics/VertexArray.h>


void sfVertexArray_append_helper (sfVertexArray* vertexArray, sfVertex* vertex)
{
    sfVertexArray_append (vertexArray, *vertex);
}


void sfVertexArray_getBounds_helper (sfVertexArray* vertexArray, sfFloatRect* rect)
{
    *rect = sfVertexArray_getBounds (vertexArray);
}

