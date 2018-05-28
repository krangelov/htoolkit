#include "Types.h"
#include "Canvas.h"
#include "Font.h"
#include "Internals.h"

int osMMtoVPixels(double mm)
{
	return (int) ((mm*gdk_screen_height())/gdk_screen_height_mm());
}

int osMMtoHPixels(double mm)
{
	return (int) ((mm*gdk_screen_width())/gdk_screen_width_mm());
}

/*------------------------------------*\
|									   |
|	   Helper functions 			   |
|									   |
\*------------------------------------*/

void osInitCanvas 	(int size, int function,
					 unsigned int pcolor,
					 unsigned int bcolor,
					 int joinStyle,
					 int capStyle,
					 int lineStyle,
					 int lineCustomCount,
					 unsigned char* lineCustomDashes,
					 BOOL backDraw,
					 int fillStyle,
					 void *fill_info,
					 FontHandle font,
					 CanvasHandle canvas,
					 BOOL buffered
					)
{
	GdkRectangle clip_box;

	canvas->tile = NULL;

	osChangeCanvasPen(size,function,pcolor,bcolor,joinStyle,capStyle,lineStyle,lineCustomCount,lineCustomDashes,backDraw,fillStyle,fill_info,font,canvas);

	if (canvas->buffered == 0 && buffered)
	{
		GdkWindowObject *private = GDK_WINDOW_OBJECT(canvas->drawable);
		if (!private->paint_stack)
		{
			gdk_window_begin_paint_region(GDK_WINDOW(canvas->drawable), canvas->region);

			if (private->paint_stack)
			{
				GdkGC *tmp_gc;
				GdkDrawable *cache = NULL;

				gdk_window_get_internal_paint_info(GDK_WINDOW(canvas->drawable), &cache, NULL, NULL);

				gdk_region_get_clipbox (canvas->region, &clip_box);

				tmp_gc = gdk_gc_new (cache);
				gdk_draw_drawable(cache, tmp_gc, private->impl, clip_box.x, clip_box.y,
										0, 0, clip_box.width, clip_box.height);
				gdk_gc_unref (tmp_gc);
			}

			canvas->buffered = 1;
		}
	}

	if (canvas->buffered == 2)
	{
		gdk_region_get_clipbox (canvas->region, &clip_box);
		cairo_save(canvas->cr);
		cairo_set_source_rgba(canvas->cr, ((double) ((bcolor      ) & 0xFF))/255
								        , ((double) ((bcolor >>  8) & 0xFF))/255
								        , ((double) ((bcolor >> 16) & 0xFF))/255
								        , 1.0);
		cairo_rectangle(canvas->cr, clip_box.x, clip_box.y, clip_box.width, clip_box.height);
		cairo_fill(canvas->cr);
		cairo_restore(canvas->cr);
	}
}	/* osInitCanvas */

void osDoneCanvas (CanvasHandle canvas)
{
	if (canvas->buffered == 1)
	{
		gdk_window_end_paint(GDK_WINDOW(canvas->drawable));
		canvas->buffered = 0;
	}

	if (canvas->tile)
	{
		rfree(canvas->tile);
		canvas->tile = NULL;
	}
}	/* osDoneCanvas */

/*	Operations to create, modify, and destroy polygon shapes.
*/

PolygonHandle osCreatePolygon(int size)
{
	PolygonHandle shape = (PolygonHandle) rmalloc (sizeof(*shape) + size * sizeof (GdkPoint));
	shape->count = 0;
	shape->max_count = size;
	return shape;
}	/* osCreatePolygon */

void osAddPolygonPoint(PolygonHandle shape, int x, int y)
{
	if (shape->count > shape->max_count)
	{
		printf("Too many points in PolygonHandle\n");
		exit(1);
	}

	shape->data[shape->count].x = x;
	shape->data[shape->count].y = y;
	shape->count++;
}	/* osAddPolygonPoint */

void osDeletePolygon(PolygonHandle shape)
{
	rfree (shape);
}	/* osDeletePolygon */


/*------------------------------------*\

|	   Interface functions			   |
\*------------------------------------*/

static double dashStyles[] = {15,15};
static double dotStyles[] = {1,4};
static double dashDotStyles[] = {5,3,1,3};
static double dashDotDotStyles[] = {5,3,1,3,1,3};

static cairo_pattern_t *create_stipple(guchar stipple_data[])
{
	cairo_surface_t *surface;
	cairo_pattern_t *pattern;

	int stride;
	const int width = 8;
	const int height = 8;

	stride = cairo_format_stride_for_width (CAIRO_FORMAT_ARGB32, width);
	g_assert (stride > 0);

	surface = cairo_image_surface_create_for_data (stipple_data, CAIRO_FORMAT_ARGB32, width, height,
	                                               stride);
	pattern = cairo_pattern_create_for_surface (surface);
	cairo_surface_destroy (surface);
	cairo_pattern_set_extend (pattern, CAIRO_EXTEND_REPEAT);

	return pattern;
}

#define PREMULTIPLY(argb)                                                                          \
	((255 - ((argb) >> 24) & 0xFF) |                                                                       \
	 (((((argb) & 0xFF <<  0) >>  0) * ((255 - ((argb) >> 24) & 0xFF)) / 0xFF) << 16) |              \
	 (((((argb) & 0xFF <<  8) >>  8) * ((255 - ((argb) >> 24) & 0xFF)) / 0xFF) <<  8) |                 \
	 (((((argb) & 0xFF << 16) >> 16) * ((255 - ((argb) >> 24) & 0xFF)) / 0xFF) <<  0))

void osChangeCanvasPen(int size, int function,
					 unsigned int pcolor,
					 unsigned int bcolor,
					 int joinStyle,
					 int capStyle,
					 int lineStyle,
					 int lineCustomCount,
					 unsigned char *lineCustomDashes,
					 BOOL backDraw,
					 int fillStyle,
					 void *fill_info,
					 FontHandle font,
					 CanvasHandle canvas
					 )
{
	cairo_line_cap_t  cairoCapStyle;
	cairo_line_join_t cairoJoinStyle;

	if (canvas->cr)
	{
		canvas->backDraw = backDraw;
		canvas->pcolor = pcolor;
		canvas->bcolor = bcolor;

		cairo_set_line_width (canvas->cr, size);

		switch (joinStyle)
		{
		case 0: cairoJoinStyle = CAIRO_LINE_JOIN_BEVEL; break;
		case 1: cairoJoinStyle = CAIRO_LINE_JOIN_MITER; break;
		case 2: cairoJoinStyle = CAIRO_LINE_JOIN_ROUND; break;
		}
		cairo_set_line_join (canvas->cr, cairoJoinStyle);

		switch (capStyle)
		{
		case 0: cairoCapStyle = CAIRO_LINE_CAP_ROUND;  	break;
		case 1: cairoCapStyle = CAIRO_LINE_CAP_SQUARE;  break;
		case 2: cairoCapStyle = CAIRO_LINE_CAP_BUTT;   	break;
		}
		cairo_set_line_cap (canvas->cr, cairoCapStyle);

		switch (lineStyle)
		{
		case 1:
			cairo_set_dash (canvas->cr, dashStyles, G_N_ELEMENTS(dashStyles), 0);
			break;
		case 2:
			cairo_set_dash (canvas->cr, dotStyles, G_N_ELEMENTS(dotStyles), 0);
			break;
		case 3:
			cairo_set_dash (canvas->cr, dashDotStyles, G_N_ELEMENTS(dashDotStyles), 0);
			break;
		case 4:
			cairo_set_dash (canvas->cr, dashDotDotStyles, G_N_ELEMENTS(dashDotDotStyles), 0);
			break;
		case 5: {
			int i;
			double* dash = rmalloc(lineCustomCount*sizeof(double));
			for (i = 0; i < lineCustomCount; i++) {
				dash[i] = lineCustomDashes[i];
			}
			rfree(lineCustomDashes);
			cairo_set_dash (canvas->cr, dash, lineCustomCount, 0);
			rfree(dash);
			break;
		}
		}

		if (canvas->tile)
		{
			rfree(canvas->tile);
			canvas->tile = NULL;
		}

		guint32 pcolor_pre = PREMULTIPLY(pcolor);
		guint32 bcolor_pre = canvas->backDraw ? PREMULTIPLY(bcolor) : PREMULTIPLY(0x00000000);

		switch (fillStyle)
		{
		case 0:
			cairo_set_source_rgba(canvas->cr, ((double) ((pcolor      ) & 0xFF))/255
											, ((double) ((pcolor >>  8) & 0xFF))/255
											, ((double) ((pcolor >> 16) & 0xFF))/255
											, ((double) ((255 - (pcolor >> 24)) & 0xFF))/255);
			break;
		case 1: {
			guint32 stipple_data[8 * 8] = {
			  pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre
			};
			canvas->tile = rmalloc(8*8*sizeof(guint32));
			memcpy(canvas->tile, stipple_data, 8*8*sizeof(guint32));

			cairo_pattern_t *pattern = create_stipple(canvas->tile);
			cairo_set_source(canvas->cr, pattern);
			cairo_pattern_destroy(pattern);
			break;
		}
		case 2: {
			guint32 stipple_data[8 * 8] = {
			  pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre
			};
			canvas->tile = rmalloc(8*8*sizeof(guint32));
			memcpy(canvas->tile, stipple_data, 8*8*sizeof(guint32));

			cairo_pattern_t *pattern = create_stipple(canvas->tile);
			cairo_set_source(canvas->cr, pattern);
			cairo_pattern_destroy(pattern);
			break;
		}
		case 3: {
			guint32 stipple_data[8 * 8] = {
			  bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre
			};
			canvas->tile = rmalloc(8*8*sizeof(guint32));
			memcpy(canvas->tile, stipple_data, 8*8*sizeof(guint32));

			cairo_pattern_t *pattern = create_stipple(canvas->tile);
			cairo_set_source(canvas->cr, pattern);
			cairo_pattern_destroy(pattern);
			break;
		}
		case 4:  {
			guint32 stipple_data[8 * 8] = {
			  pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre,
			  bcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre
			};
			canvas->tile = rmalloc(8*8*sizeof(guint32));
			memcpy(canvas->tile, stipple_data, 8*8*sizeof(guint32));

			cairo_pattern_t *pattern = create_stipple(canvas->tile);
			cairo_set_source(canvas->cr, pattern);
			cairo_pattern_destroy(pattern);
			break;
		}
		case 5: {
			guint32 stipple_data[8 * 8] = {
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre,
			  bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre
			};
			canvas->tile = rmalloc(8*8*sizeof(guint32));
			memcpy(canvas->tile, stipple_data, 8*8*sizeof(guint32));

			cairo_pattern_t *pattern = create_stipple(canvas->tile);
			cairo_set_source(canvas->cr, pattern);
			cairo_pattern_destroy(pattern);
			break;
		}
		case 6: {
			guint32 stipple_data[8 * 8] = {
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre,
			  pcolor_pre, pcolor_pre, pcolor_pre, pcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre, bcolor_pre
			};
			canvas->tile = rmalloc(8*8*sizeof(guint32));
			memcpy(canvas->tile, stipple_data, 8*8*sizeof(guint32));

			cairo_pattern_t *pattern = create_stipple(canvas->tile);
			cairo_set_source(canvas->cr, pattern);
			cairo_pattern_destroy(pattern);
			break;
		}
		case 7:
		case 8: {
			cairo_set_source(canvas->cr, (cairo_pattern_t *) fill_info);
			cairo_pattern_set_extend (cairo_get_source(canvas->cr), CAIRO_EXTEND_REPEAT);
			cairo_pattern_destroy((cairo_pattern_t *) fill_info);
			break;
		}
		case 9: {
			gdk_cairo_set_source_pixbuf (canvas->cr, ((BitmapHandle) fill_info)->pixbuf, 0, 0);
			cairo_pattern_set_extend (cairo_get_source(canvas->cr), CAIRO_EXTEND_REPEAT);
			break;
		}
		}

		switch (function)
		{
			case 0:
				cairo_set_operator (canvas->cr, CAIRO_OPERATOR_OVER);
				break;
			case 1:
				cairo_set_operator (canvas->cr, CAIRO_OPERATOR_DIFFERENCE);				
				break;
			case 2:
				cairo_set_operator (canvas->cr, CAIRO_OPERATOR_XOR);
				break;
		}

		canvas->theFont = font;
		pango_layout_set_font_description(canvas->layout, font->font_descr);
	}
}

GradientHandle osNewLinearGradient(int x1, int y1, int x2, int y2)
{
	return cairo_pattern_create_linear (x1, y1, x2, y2);
}

GradientHandle osNewRadialGradient(int x1, int y1, int radius1, int x2, int y2, int radius2)
{
	return cairo_pattern_create_radial (x1, y1, radius1, x2, y2, radius2);
}

void osAddGradientStop(GradientHandle gradient, double offset, unsigned int color) {
	cairo_pattern_add_color_stop_rgba (gradient,
                                       offset,
                                       ((double) ((color      ) & 0xFF))/255,
                                       ((double) ((color >>  8) & 0xFF))/255,
                                       ((double) ((color >> 16) & 0xFF))/255,
                                       ((double) ((255 - (color >> 24)) & 0xFF))/255);
}

void osRotateCanvas(double angle, CanvasHandle canvas)
{
	if (canvas->cr)
		cairo_rotate (canvas->cr, angle);
};

void osScaleCanvas(double dScaleX, double dScaleY, CanvasHandle canvas)
{
	if (canvas->cr)
		cairo_scale (canvas->cr, dScaleX, dScaleY);
};

void osShearCanvas(double dShearX, double dShearY, CanvasHandle canvas)
{
	if (canvas->cr) {
		cairo_matrix_t matrix;
		cairo_matrix_init (&matrix,
		                   1.0, dShearX,
		                   dShearY, 1.0,
		                   0.0, 0.0);
		cairo_transform (canvas->cr, &matrix);
	}
};

void osTranslateCanvas(double dDeltaX, double dDeltaY, CanvasHandle canvas)
{
	if (canvas->cr)
		cairo_translate (canvas->cr, dDeltaX, dDeltaY);
};

void osDrawPoint(int x, int y, CanvasHandle canvas)
{
	if (canvas->cr) {
		cairo_arc (canvas->cr, x, y, cairo_get_line_width(canvas->cr), 0, 2 * M_PI);
		cairo_fill(canvas->cr);
	}
}	/* osDrawPoint */

void osDrawLine(int startx, int starty, int endx, int endy, CanvasHandle canvas)
{
	if (canvas->cr) {
		cairo_move_to(canvas->cr, startx, starty);
		cairo_line_to(canvas->cr, endx, endy);
		cairo_stroke(canvas->cr);
	}
}

void osDrawCurve(int left, int top, int right, int bot, float from, float to, gboolean clockwise,CanvasHandle canvas)
{
	if (canvas->cr) {
		cairo_save (canvas->cr);
		cairo_translate (canvas->cr, (left + right) / 2., (top + bot) / 2.);
		cairo_scale (canvas->cr, (right-left) / 2., (bot-top) / 2.);
		cairo_arc (canvas->cr, 0, 0, 1, from, to);
		cairo_restore (canvas->cr);
		cairo_stroke(canvas->cr);
	}
}

void osFillCurve(int left, int top, int right, int bot, float from, float to, gboolean clockwise,CanvasHandle canvas)
{
	if (canvas->cr) {
		cairo_save (canvas->cr);
		cairo_translate (canvas->cr, (left + right) / 2., (top + bot) / 2.);
		cairo_scale (canvas->cr, (right-left) / 2., (bot-top) / 2.);
		cairo_move_to(canvas->cr, 0, 0);
		cairo_arc (canvas->cr, 0, 0, 1, from, to);
		cairo_close_path(canvas->cr);
		cairo_restore (canvas->cr);
		cairo_fill(canvas->cr);
	}
}

static void osSetupFont(CanvasHandle canvas, int len)
{
	if (canvas->theFont->style & (FONT_UNDERLINED | FONT_STRIKED))
	{
		PangoAttribute *attr;
		PangoAttrList *attr_list;

		attr_list = pango_layout_get_attributes(canvas->layout);

		if (!attr_list)
		{
			attr_list = pango_attr_list_new();
			pango_layout_set_attributes(canvas->layout, attr_list);
			pango_attr_list_unref(attr_list);
			
			attr_list = pango_layout_get_attributes(canvas->layout);
		}

		if (canvas->backDraw) {
			attr = pango_attr_background_new(((canvas->bcolor      ) & 0xFF)*257,
			                                 ((canvas->bcolor >>  8) & 0xFF)*257,
			                                 ((canvas->bcolor >> 16) & 0xFF)*257);
			attr->start_index = 0;
			attr->end_index = len;

			pango_attr_list_change(attr_list, attr);
			pango_attribute_destroy(attr);

			/*attr = pango_attr_background_alpha_new(((255 - (canvas->bcolor >> 24)) & 0xFF)*257);
			attr->start_index = 0;
			attr->end_index = len;

			pango_attr_list_change(attr_list, attr);
			pango_attribute_destroy(attr);*/
		}

		if (canvas->theFont->style & FONT_UNDERLINED)
		{
			attr = pango_attr_underline_new(PANGO_UNDERLINE_SINGLE);
			attr->start_index = 0;
			attr->end_index = len;

			pango_attr_list_change(attr_list, attr);
			pango_attribute_destroy(attr);
		}

		if (canvas->theFont->style & FONT_STRIKED)
		{
			attr = pango_attr_strikethrough_new(gtk_true());
			attr->start_index = 0;
			attr->end_index = len;

			pango_attr_list_change(attr_list, attr);
			pango_attribute_destroy(attr);
		}
	}
}

void osDrawChar (int x, int y, char c, CanvasHandle canvas)
{
	if (canvas->cr)
	{
		pango_layout_set_text(canvas->layout, &c, 1);

		pango_cairo_update_layout (canvas->cr, canvas->layout);
		osSetupFont(canvas, 1);

        cairo_move_to (canvas->cr, x, y - ((double) pango_font_metrics_get_ascent(canvas->theFont->metrics)) / PANGO_SCALE);
        pango_cairo_show_layout (canvas->cr, canvas->layout);

		pango_layout_set_text(canvas->layout, NULL, 0);
		
		cairo_stroke(canvas->cr);
	}
}	/* osDrawChar */

void osDrawString (int x, int y, char *string, CanvasHandle canvas)
{
	if (canvas->cr)
	{
		pango_layout_set_text(canvas->layout, string, strlen(string));

		pango_cairo_update_layout (canvas->cr, canvas->layout);
		osSetupFont(canvas, strlen(string));

        cairo_move_to (canvas->cr, x, y - ((double) pango_font_metrics_get_ascent(canvas->theFont->metrics)) / PANGO_SCALE);
        pango_cairo_show_layout (canvas->cr, canvas->layout);

		pango_layout_set_text(canvas->layout, NULL, 0);
		
		cairo_stroke(canvas->cr);
	}
}	/* osDrawString */

void osDrawRect(int left, int top, int right, int bot, CanvasHandle canvas)
{
	if (canvas->cr) {
		cairo_rectangle(canvas->cr, left, top, right-left+1, bot-top+1);
		cairo_stroke(canvas->cr);
	}
}	/* osDrawRect */

void osFillRect(int left, int top, int right, int bot, CanvasHandle canvas)
{
	if (canvas->cr) {
		cairo_rectangle(canvas->cr, left, top, right-left+1, bot-top+1);
		cairo_fill(canvas->cr);
	}
}	/* osFillRect */

void osDrawOval (int left, int top, int right, int bot, CanvasHandle canvas)
{
	if (canvas->cr) {
		cairo_save (canvas->cr);
		cairo_translate (canvas->cr, (left + right) / 2., (top + bot) / 2.);
		cairo_scale (canvas->cr, (right-left) / 2., (bot-top) / 2.);
		cairo_arc (canvas->cr, 0, 0, 1, 0, 2 * M_PI);
		cairo_restore (canvas->cr);
		cairo_stroke(canvas->cr);
	}
}	/* osDrawOval */

void osFillOval (int left, int top, int right, int bot, CanvasHandle canvas)
{
	if (canvas->cr) {
		cairo_save (canvas->cr);
		cairo_translate (canvas->cr, (left + right) / 2., (top + bot) / 2.);
		cairo_scale (canvas->cr, (right-left) / 2., (bot-top) / 2.);
		cairo_arc (canvas->cr, 0., 0., 1., 0., 2 * M_PI);
		cairo_restore (canvas->cr);
		cairo_fill(canvas->cr);
	}
}	/* osFillOval */

void osDrawPolyline(PolygonHandle arr, CanvasHandle canvas)
{
	if (arr->count == 0)
		return;

	if (canvas->cr) {
		int i;

		cairo_move_to(canvas->cr, arr->data[0].x, arr->data[0].y);
		for (i = 1; i < arr->count; i++) {
			cairo_line_to(canvas->cr, arr->data[i].x, arr->data[i].y);
		}
		cairo_stroke(canvas->cr);
	}
}	/* osDrawPolyline */


void osDrawPolygon(PolygonHandle arr, CanvasHandle canvas)
{
	if (arr->count == 0)
		return;

	if (canvas->cr) {
		int i;

		cairo_move_to(canvas->cr, arr->data[0].x, arr->data[0].y);
		for (i = 1; i < arr->count; i++) {
			cairo_line_to(canvas->cr, arr->data[i].x, arr->data[i].y);
		}
		cairo_close_path(canvas->cr);
		cairo_stroke(canvas->cr);
	}
}	/* osDrawPolygon */


void osFillPolygon (PolygonHandle arr, CanvasHandle canvas)
{
	if (arr->count == 0)
		return;

	if (canvas->cr) {
		int i;

		cairo_move_to(canvas->cr, arr->data[0].x, arr->data[0].y);
		for (i = 1; i < arr->count; i++) {
			cairo_line_to(canvas->cr, arr->data[i].x, arr->data[i].y);
		}
		cairo_close_path(canvas->cr);
		cairo_fill(canvas->cr);
	}
}	/* osFillPolygon */

CanvasHandle osGetTemporaryCanvas()
{
	CanvasHandle canvas;
	PangoContext *pango_context;

	pango_context = pango_context_new();

	canvas = rmalloc(sizeof(*canvas));
	memset(canvas, 0, sizeof(*canvas));
	canvas->layout = pango_layout_new(pango_context);
	canvas->buffered = 0;

	return canvas;
}	/* osGetTemporaryCanvas */

void osReleaseTemporaryCanvas (CanvasHandle canvas)
{
	g_object_unref(canvas->layout);
	rfree(canvas);
}	/* osReleaseTemporaryCanvas */

void osDrawBitmap (int destx, int desty, BitmapHandle bmp, CanvasHandle canvas)
{
	if (canvas->cr)
    {
		cairo_save (canvas->cr);
		gdk_cairo_set_source_pixbuf (canvas->cr, bmp->pixbuf, destx, desty);
		cairo_paint (canvas->cr);
		cairo_restore (canvas->cr);
    }
}	/* osDrawBitmap */

static unsigned int gdk_color_to_rgb(GdkColor color)
{
	return (color.red/257) | ((color.green/257) << 8) | ((color.blue/257)  << 16);
}

unsigned int osGetDialogColor()
{
	GtkStyle *style = gtk_style_new();
	int rgb = gdk_color_to_rgb(style->bg[GTK_STATE_NORMAL]);
	gtk_style_unref(style);
	return rgb;
}

unsigned int osGetWindowColor()
{
	GtkStyle *style = gtk_style_new();
	int rgb = gdk_color_to_rgb(style->base[GTK_STATE_NORMAL]);
	gtk_style_unref(style);
	return rgb;
}

unsigned int osGetTextColor()
{
	GtkStyle *style = gtk_style_new();
	int rgb = gdk_color_to_rgb(style->fg[GTK_STATE_NORMAL]);
	gtk_style_unref(style);
	return rgb;
}
