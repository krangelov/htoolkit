#include "Font.h"
#include "Internals.h"

char *osGetAvailableFontNames()
{
	printf("osGetAvailableFontNames\n");
	return NULL;
}

int  *osGetAvailableFontVariants(char *szFontName, int nLow, int nHight)
{
	printf("osGetAvailableFontVariants\n");
	return NULL;

}

FontHandle osCreateFont(char *face, int size, int weight, int style)
{
	int traits = (style == 1 || style == 2) ? NSItalicFontMask : NSUnitalicFontMask;

	NSFontManager *fontManager = [NSFontManager sharedFontManager];
	return [fontManager fontWithFamily: [NSString stringWithUTF8String: face]
                                traits: traits
                                weight: ((weight-100)*15) / (900-100)
                                  size: size];
}

void osDeleteFont(FontHandle font)
{
	printf("osDeleteFont\n");
}	/* osDeleteFont */

void osGetFontMetrics(FontHandle font, CanvasHandle canvas, int *ascent, int *descent, int *maxwidth, int *leading)
{
	printf("osGetFontMetrics\n");
}	/* osGetFontMetrics */

void osGetPenFontMetrics(CanvasHandle canvas, int *ascent, int *descent, int *maxwidth, int *leading)
{
	printf("osGetPenFontMetrics\n");
}	/* osGetPenFontMetrics */


int osGetStringWidth(char *string, CanvasHandle canvas)
{
	printf("osGetStringWidth\n");
	return 0;
}	/* osGetStringWidth */

int osGetCharWidth(char ch, CanvasHandle canvas)
{
	printf("osGetCharWidth\n");
	return 0;
}	/* osGetCharWidth */

int osGetFontStringWidth(char *string, FontHandle font, CanvasHandle canvas)
{
	printf("osGetFontStringWidth\n");
	return 0;
}	/* osGetFontStringWidth */

int osGetFontCharWidth(char ch, FontHandle font, CanvasHandle canvas)
{
	printf("osGetFontCharWidth\n");
	return 0;
}	/* osGetFontCharWidth */

void osDefaultFontDef(char **face, int *size, int *weight, int *style)
{
	NSFont* font = [NSFont systemFontOfSize: 0];
	*face = strdup([font.fontName UTF8String]);
	*size = font.pointSize;
	*weight = 100;
	*style = 0;
}

void osSerifFontDef(char **face, int *size, int *weight, int *style)
{
	printf("osSerifFontDef\n");
};

void osSansSerifFontDef(char **face, int *size, int *weight, int *style)
{
	printf("osSansSerifFontDef\n");
};

void osSmallFontDef(char **face, int *size, int *weight, int *style)
{
	printf("osSmallFontDef\n");
};

void osNonProportionalFontDef(char **face, int *size, int *weight, int *style)
{
	printf("osNonProportionalFontDef\n");
};

void osSymbolFontDef(char **face, int *size, int *weight, int *style)
{
	printf("osSymbolFontDef\n");
};
