#include "Font.h"
#include "Internals.h"

char *osGetAvailableFontNames()
{
	int i, nCount, nBufferSize;
	PangoFontMap *fontmap;
	PangoFontFamily **families;
	char *buffer, *p;

	fontmap = pango_cairo_font_map_get_default();

	pango_font_map_list_families(fontmap, &families, &nCount);

	nBufferSize = 0;
	for (i = 0; i < nCount; i++)
	{
		nBufferSize += strlen(pango_font_family_get_name(families[i]))+1;
	}

	buffer = rmalloc(nBufferSize+1); p = buffer;
	for (i = 0; i < nCount; i++)
	{
			strcpy(p, pango_font_family_get_name(families[i]));
			p += strlen(p)+1;
	}
	*p = 0;

	g_free(families);

	return buffer;
}

int  *osGetAvailableFontVariants(char *szFontName, int nLow, int nHight)
{
	int i, nCount;
	PangoFontMap *fontmap;
	PangoFontFamily **families, *family;
	PangoFontFace **faces;
	int *buffer, *p;

	fontmap = pango_cairo_font_map_get_default();

	pango_font_map_list_families(fontmap, &families, &nCount);

	family = NULL;
	for (i = 0; i < nCount; i++)
	{
		if (strcmp(pango_font_family_get_name(families[i]), szFontName) == 0)
		{
			family = families[i];
			break;
		}
	}

	g_free(families);

	if (family)
	{
		pango_font_family_list_faces(family, &faces, &nCount);

		buffer = rmalloc(sizeof(int)*(nCount*3+1)); p = buffer;
		for (i = 0; i < nCount; i++)
		{
			PangoFontDescription *desc = pango_font_face_describe(faces[i]);

			*(p++) = pango_font_description_get_weight(desc);
			switch (pango_font_description_get_style(desc))
			{
			case PANGO_STYLE_NORMAL:   *(p++) =  FONT_NORMAL; break;
			case PANGO_STYLE_OBLIQUE:  *(p++) =  FONT_ITALIC;     break;
			case PANGO_STYLE_ITALIC:      *(p++) =  FONT_OBLIQUE; break;
			default:                                        *(p++) = FONT_NORMAL;  break;
			}
			*(p++) =  0;

			pango_font_description_free(desc);
		}
		*(p++) = 0;

		return buffer;
	}

	return NULL;

}

FontHandle osCreateFont(char *face, int size, int weight, int style)
{
	FontHandle font;
	PangoFont *pango_font;

	font = rmalloc(sizeof(*font));

	font->font_descr = pango_font_description_new();
	pango_font_description_set_family(font->font_descr,face);
	pango_font_description_set_weight(font->font_descr,weight);
	pango_font_description_set_style(font->font_descr,style & FONT_TYPE_MASK);
	pango_font_description_set_size(font->font_descr, size*PANGO_SCALE);

	pango_font = pango_font_map_load_font
			( pango_cairo_font_map_get_default()
			, NULL
			, font->font_descr
			);
	font->metrics = pango_font_get_metrics(pango_font, NULL);

	font->style = style;

	return font;
}

void osDeleteFont(FontHandle font)
{
	pango_font_metrics_unref(font->metrics);
	pango_font_description_free(font->font_descr);
	rfree(font);
}	/* osDeleteFont */

void osGetFontMetrics(FontHandle font, CanvasHandle canvas, int *ascent, int *descent, int *maxwidth, int *leading)
{
	*ascent = pango_font_metrics_get_ascent(font->metrics)/PANGO_SCALE;
	*descent = pango_font_metrics_get_descent(font->metrics)/PANGO_SCALE;
	*maxwidth = pango_font_metrics_get_approximate_char_width(font->metrics)/PANGO_SCALE;
	*leading = 2; /* FIXME */
}	/* osGetFontMetrics */


void osGetPenFontMetrics(CanvasHandle canvas, int *ascent, int *descent, int *maxwidth, int *leading)
{
	*ascent = pango_font_metrics_get_ascent(canvas->theFont->metrics)/PANGO_SCALE;
	*descent = pango_font_metrics_get_descent(canvas->theFont->metrics)/PANGO_SCALE;
	*maxwidth = pango_font_metrics_get_approximate_char_width(canvas->theFont->metrics)/PANGO_SCALE;
	*leading = 2; /* FIXME */
}	/* osGetPenFontMetrics */


int osGetStringWidth(char *string, CanvasHandle canvas)
{
	int width;

	pango_layout_set_text(canvas->layout, string, -1);
	pango_layout_get_pixel_size(canvas->layout, &width, NULL);
	pango_layout_set_text(canvas->layout, NULL, 0);

	return width;
}	/* osGetStringWidth */

int osGetCharWidth(char ch, CanvasHandle canvas)
{
	int width;

	pango_layout_set_text(canvas->layout, &ch, 1);
	pango_layout_get_pixel_size(canvas->layout, &width, NULL);
	pango_layout_set_text(canvas->layout, NULL, 0);

	return width;
}	/* osGetCharWidth */

int osGetFontStringWidth(char *string, FontHandle font, CanvasHandle canvas)
{
	int width;

	pango_layout_set_font_description(canvas->layout, font->font_descr);
	pango_layout_set_text(canvas->layout, string, -1);
	pango_layout_get_pixel_size(canvas->layout, &width, NULL);
	pango_layout_set_text(canvas->layout, NULL, 0);
	pango_layout_set_font_description(canvas->layout, canvas->theFont->font_descr);

	return width;
}	/* osGetFontStringWidth */

int osGetFontCharWidth(char ch, FontHandle font, CanvasHandle canvas)
{
	int width;

	pango_layout_set_font_description(canvas->layout, font->font_descr);
	pango_layout_set_text(canvas->layout, &ch, 1);
	pango_layout_get_pixel_size(canvas->layout, &width, NULL);
	pango_layout_set_text(canvas->layout, NULL, 0);
	pango_layout_set_font_description(canvas->layout, canvas->theFont->font_descr);

	return width;
}	/* osGetFontCharWidth */

void osDefaultFontDef(char **face, int *size, int *weight, int *style)
{
	GtkStyle *gtkstyle = gtk_style_new();
	*face    = strdup(pango_font_description_get_family (gtkstyle->font_desc));
	*size    = pango_font_description_get_size (gtkstyle->font_desc)/PANGO_SCALE;
	*weight = pango_font_description_get_weight(gtkstyle->font_desc);

	switch (pango_font_description_get_style(gtkstyle->font_desc))
	{
	case PANGO_STYLE_NORMAL:   *style = FONT_NORMAL; break;
	case PANGO_STYLE_OBLIQUE:  *style = FONT_ITALIC;     break;
	case PANGO_STYLE_ITALIC:      *style = FONT_OBLIQUE; break;
	default:                                        *style = FONT_NORMAL; break;
	}
	gtk_style_unref(gtkstyle);
}

void osSerifFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("times");
	*size    = 10;
	*weight  = 400;
	*style   = 0;
};

void osSansSerifFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("Sans");
	*size    = 10;
	*weight  = 500;
	*style   = 0;
};

void osSmallFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("helvetica");
	*size    = 7;
	*weight  = 500;
	*style   = 0;
};

void osNonProportionalFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("fixed");
	*size    = 10;
	*weight  = 400;
	*style   = 0;
};

void osSymbolFontDef(char **face, int *size, int *weight, int *style)
{
	*face    = strdup("symbol");
	*size    = 10;
	*weight  = 400;
	*style   = 0;
};
