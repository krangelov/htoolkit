#ifndef FONT_H
#define FONT_H

#include "Types.h"

// the following types are exqlusive
#define FONT_NORMAL    0x00
#define FONT_ITALIC    0x01
#define FONT_OBLIQUE   0x02

// types mask
#define FONT_TYPE_MASK 0x03

#define FONT_UNDERLINED 0x04
#define FONT_STRIKED    0x08

extern char *osGetAvailableFontNames();
extern int  *osGetAvailableFontVariants(char *szFontName, int nLow, int nHigh);

extern FontHandle osCreateFont(char *face, int size, int weight, int style);
extern void osDeleteFont(FontHandle font);

extern void osGetFontMetrics(FontHandle font,CanvasHandle canvas, int *ascent, int *descent, int *maxwidth, int *leading);
extern void osGetPenFontMetrics(CanvasHandle canvas, int *ascent, int *descent, int *maxwidth, int *leading);

extern int osGetStringWidth(char *,CanvasHandle canvas);
extern int osGetCharWidth(char,CanvasHandle canvas);
extern int osGetFontStringWidth(char *,FontHandle font,CanvasHandle canvas);
extern int osGetFontCharWidth(char,FontHandle font,CanvasHandle canvas);

extern void osDefaultFontDef(char **face, int *size, int *weight, int *style);
extern void osSerifFontDef(char **face, int *size, int *weight, int *style);
extern void osSansSerifFontDef(char **face, int *size, int *weight, int *style);
extern void osSmallFontDef(char **face, int *size, int *weight, int *style);
extern void osNonProportionalFontDef(char **face, int *size, int *weight, int *style);
extern void osSymbolFontDef(char **face, int *size, int *weight, int *style);

#endif
