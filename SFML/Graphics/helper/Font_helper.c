#include <SFML/Graphics/Font.h>


void sfFont_getGlyph_helper (sfFont* font, sfUint32 codePoint, unsigned int characterSize, sfBool bold, sfGlyph* glyph)
{
    *glyph = sfFont_getGlyph (font, codePoint, characterSize, bold);
}

