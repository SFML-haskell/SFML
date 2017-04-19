#include <SFML/Graphics/Font.h>


void sfFont_getGlyph_helper (sfFont* font, sfUint32 codePoint, unsigned int characterSize, float outline, sfBool bold, sfGlyph* glyph)
{
    *glyph = sfFont_getGlyph (font, codePoint, characterSize, bold, outline);
}


void sfFont_getInfo_helper (sfFont* font, sfFontInfo* info)
{
    *info = sfFont_getInfo (font);
}
