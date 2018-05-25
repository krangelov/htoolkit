#include "Font.h"
#include "Internals.h"

static int CALLBACK CalcNamesBufferSize(ENUMLOGFONT FAR * lpelf,	/* pointer to logical-font  data */
								   NEWTEXTMETRIC FAR * lpntm,		/* pointer to physical-font data */
								   int fontType,					/* type of font */
								   LPARAM lParam					/* address of application-defined data	*/
								  )
{
	*((int*) lParam) += strlen(lpelf->elfLogFont.lfFaceName) + 1;
	return 1;
}

static int CALLBACK FillNamesBuffer(ENUMLOGFONT FAR * lpelf,		/* pointer to logical-font  data */
							   		NEWTEXTMETRIC FAR * lpntm,		/* pointer to physical-font data */
							   		int fontType,					/* type of font */
							   		LPARAM lParam					/* address of application-defined data	*/
							  		)
{
	char **pBuffer = (char **) lParam;

	strcpy(*pBuffer,lpelf->elfLogFont.lfFaceName);
	*pBuffer += strlen(lpelf->elfLogFont.lfFaceName) + 1;

	return 1;
}

typedef struct
{
	int nLow, nHigh;
	int nScale;
	int nCount;
	int *buffer;
	int nWeight;
	int nItalic;
	BOOL accessed;
} EnumSizesInfo;

static int CALLBACK CalcFontVariantsCount(ENUMLOGFONT FAR * lpelf,		/* pointer to logical-font  data */
								   NEWTEXTMETRIC FAR * lpntm,		/* pointer to physical-font data */
								   int fontType,					/* type of font */
								   LPARAM lParam					/* address of application-defined data	*/
								  )
{
	EnumSizesInfo *pInfo = (EnumSizesInfo *) lParam;

	if (pInfo->nWeight != lpelf->elfLogFont.lfWeight || pInfo->nItalic != lpelf->elfLogFont.lfItalic)
	{
		int h = ((lpntm->tmHeight - lpntm->tmInternalLeading)*72)/pInfo->nScale;

		if ((fontType == TRUETYPE_FONTTYPE) || (h >= pInfo->nLow && h <= pInfo->nHigh))
			pInfo->nCount++;

		pInfo->nWeight = lpelf->elfLogFont.lfWeight;
		pInfo->nItalic = lpelf->elfLogFont.lfItalic;

		pInfo->accessed = TRUE;
	}

	return 1;
}

static int CALLBACK FillVariantsBuffer(ENUMLOGFONT FAR * lpelf,		/* pointer to logical-font  data */
							   		NEWTEXTMETRIC FAR * lpntm,		/* pointer to physical-font data */
							   		int fontType,					/* type of font */
							   		LPARAM lParam					/* address of application-defined data	*/
							  		)
{
	EnumSizesInfo *pInfo = (EnumSizesInfo *) lParam;

	if (pInfo->nWeight != lpelf->elfLogFont.lfWeight || pInfo->nItalic != lpelf->elfLogFont.lfItalic)
	{
		int h = ((lpntm->tmHeight - lpntm->tmInternalLeading)*72)/pInfo->nScale;

		if ((fontType == TRUETYPE_FONTTYPE) || (h >= pInfo->nLow && h <= pInfo->nHigh))
		{
			*(pInfo->buffer++) = lpelf->elfLogFont.lfWeight;
			*(pInfo->buffer++) = (lpelf->elfLogFont.lfItalic) ? 1 : 0;
			*(pInfo->buffer++) = (fontType == TRUETYPE_FONTTYPE) ? 0 : h;
		}

		pInfo->nWeight = lpelf->elfLogFont.lfWeight;
		pInfo->nItalic = lpelf->elfLogFont.lfItalic;

		pInfo->accessed = TRUE;
	}

	return 1;
}

char *osGetAvailableFontNames()
{
	HDC hDC;
	int bufferSize;
	char *buffer, *p;

	hDC = CreateDC ("DISPLAY", NULL, NULL, NULL);
	if (!hDC)
	{
		printf("CreateDC returned NULL.\n");
		exit(1);
	}

	EnumFontFamilies (hDC, NULL, (FONTENUMPROC) CalcNamesBufferSize, (LPARAM) &bufferSize);

	buffer = rmalloc(bufferSize+1); p = buffer;
	EnumFontFamilies (hDC, NULL, (FONTENUMPROC) FillNamesBuffer,     (LPARAM) &p);
	*p = 0;

	DeleteDC (hDC);

	return buffer;
}

int *osGetAvailableFontVariants(char *szFontName, int nLow, int nHigh)
{
	HDC hDC;
	int *buffer;
	EnumSizesInfo info;

	hDC = CreateDC ("DISPLAY", NULL, NULL, NULL);
	if (!hDC)
	{
		printf("CreateDC returned NULL.\n");
		exit(1);
	}

	info.nLow   = nLow;
	info.nHigh  = nHigh;
	info.nCount = 0;
	info.buffer = NULL;
	info.nScale = GetDeviceCaps (hDC, LOGPIXELSY);
	info.accessed = FALSE;

	info.nWeight = -1;
	info.nItalic = -1;
	EnumFontFamilies (hDC, szFontName, (FONTENUMPROC) CalcFontVariantsCount, (LPARAM) &info);

	if (!info.accessed)
		return NULL;

	buffer = rmalloc((info.nCount*3+1)*sizeof(int)); info.buffer = buffer;

	info.nWeight = -1;
	info.nItalic = -1;
	EnumFontFamilies (hDC, szFontName, (FONTENUMPROC) FillVariantsBuffer, (LPARAM) &info);

	*info.buffer = 0;

	DeleteDC (hDC);
	return buffer;
}

FontHandle osCreateFont(char *face, int size, int weight, int style)
{
	LOGFONT lf;
	HDC hDC;

	if (style & FONT_OBLIQUE)
		return NULL;

	if (weight < 100 || weight > 900)
		return NULL;

	hDC = CreateDC ("DISPLAY", NULL, NULL, NULL);
	size = (size * GetDeviceCaps(hDC, LOGPIXELSY)) / 72;
	DeleteDC(hDC);

	lf.lfHeight         = -size;
	lf.lfWeight         = weight;
	lf.lfItalic         = (style & FONT_TYPE_MASK)  != 0;
	lf.lfUnderline      = (style & FONT_UNDERLINED) != 0;
	lf.lfStrikeOut      = (style & FONT_STRIKED)    != 0;
	lf.lfWidth          = 0;
	lf.lfEscapement     = 0;
	lf.lfOrientation    = 0;
	lf.lfCharSet        = DEFAULT_CHARSET;
	lf.lfOutPrecision   = OUT_DEFAULT_PRECIS;
	lf.lfClipPrecision  = CLIP_DEFAULT_PRECIS;
	lf.lfQuality        = DEFAULT_QUALITY;
	lf.lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
	strcpy(lf.lfFaceName, face);

	return CreateFontIndirect(&lf);
}	/* osCreateFont */

void osDeleteFont(FontHandle font)
{
	DeleteObject(font);
}	/* osDeleteFont */

void osGetFontMetrics(FontHandle font, CanvasHandle canvas, int *ascent, int *descent, int *maxwidth, int *leading)
{
	HDC hDC;
	HFONT hOldFont;
	TEXTMETRIC tm;

	if (canvas != NULL)
		hDC = canvas->hDC;
	  else
		hDC = CreateDC ("DISPLAY", NULL, NULL, NULL);

	hOldFont = SelectObject(hDC, font);

	GetTextMetrics (hDC, &tm);

	*ascent = tm.tmAscent - tm.tmInternalLeading;
	*descent = tm.tmDescent;
	*maxwidth = tm.tmMaxCharWidth;
	*leading = tm.tmInternalLeading + tm.tmExternalLeading;

	SelectObject (hDC, hOldFont);

	if (canvas == NULL)
		DeleteDC(hDC);
}	/* osGetFontInfo */

void osGetPenFontMetrics (CanvasHandle canvas, int *ascent, int *descent, int *maxwidth, int *leading)
{
	TEXTMETRIC tm;

	GetTextMetrics (canvas->hDC, &tm);

	*ascent = tm.tmAscent - tm.tmInternalLeading;
	*descent = tm.tmDescent;
	*maxwidth = tm.tmMaxCharWidth;
	*leading = tm.tmInternalLeading + tm.tmExternalLeading;
}	/* osGetPenFontMetrics */

int osGetStringWidth (char *string, CanvasHandle canvas)
{
	SIZE sz;
	GetTextExtentPoint32 (canvas->hDC, string, strlen(string), &sz);
	return sz.cx;
}	/* osGetStringWidth */

int osGetCharWidth (char ch, CanvasHandle canvas)
{
	SIZE sz;
	GetTextExtentPoint32 (canvas->hDC, &ch, 1, &sz);
	return sz.cx;
}	/* osGetCharWidth */

int osGetFontStringWidth (char *string, FontHandle font, CanvasHandle canvas)
{
	HDC hDC;
	HFONT hOldFont;
	SIZE sz;

	if (canvas != NULL)
		hDC = canvas->hDC;
	else
		hDC = CreateDC ("DISPLAY", NULL, NULL, NULL);

	hOldFont = SelectObject(hDC, font);
	GetTextExtentPoint32 (hDC, string, strlen(string), &sz);
	SelectObject (hDC, hOldFont);

	if (canvas == NULL)
		DeleteDC(hDC);

	return sz.cx;
}	/* osGetStringWidth */

int osGetFontCharWidth (char ch, FontHandle font, CanvasHandle canvas)
{
	HDC hDC;
	HFONT hOldFont;
	SIZE sz;

	if (canvas != NULL)
		hDC = canvas->hDC;
	  else
		hDC = CreateDC ("DISPLAY", NULL, NULL, NULL);

	hOldFont = SelectObject (hDC, font);
	GetTextExtentPoint32 (hDC, &ch, 1, &sz);
	SelectObject (hDC, hOldFont);

	if (canvas == NULL)
		DeleteDC(hDC);

	return sz.cx;
}	/* osGetCharWidth */

void osDefaultFontDef(char **face, int *size, int *weight, int *style)
{
	LOGFONT lf;
	GetObject(GetStockObject(DEFAULT_GUI_FONT), sizeof(LOGFONT), &lf);

	*face    = strdup(lf.lfFaceName);
	*size    = -lf.lfHeight;
	*weight  = lf.lfWeight;

	*style   = 0;
	if (lf.lfItalic)    *style |= FONT_ITALIC;
	if (lf.lfUnderline) *style |= FONT_UNDERLINED;
	if (lf.lfStrikeOut) *style |= FONT_STRIKED;
}

void osSerifFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("Times New Roman");
	*size    = 10;
	*weight  = 400;
	*style   = 0;
};

void osSansSerifFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("Arial");
	*size    = 10;
	*weight  = 400;
	*style   = 0;
};

void osSmallFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("Small Fonts");
	*size    = 7;
	*weight  = 400;
	*style   = 0;
};

void osNonProportionalFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("Courier New");
	*size    = 10;
	*weight  = 400;
	*style   = 0;
};

void osSymbolFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("Symbol");
	*size    = 10;
	*weight  = 400;
	*style   = 0;
};
