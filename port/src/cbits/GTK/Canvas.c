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
					 int hatchStyle,
					 BitmapHandle hatchBitmap,
					 FontHandle font,
					 CanvasHandle canvas,
					 BOOL buffered
					)
{
	GdkRectangle clip_box;

	canvas->lineCustomCount = lineCustomCount;
	canvas->lineCustomDashes = lineCustomDashes;
	canvas->backDraw = backDraw;
	canvas->tile = NULL;

	canvas->penColor.pixel = 0;
	canvas->penColor.red   = ((pcolor      ) & 0xFF)*257;
	canvas->penColor.green = ((pcolor >>  8) & 0xFF)*257;
	canvas->penColor.blue  = ((pcolor >> 16) & 0xFF)*257;

	canvas->backColor.pixel = 0;
	canvas->backColor.red   = ((bcolor      ) & 0xFF)*257;
	canvas->backColor.green = ((bcolor >>  8) & 0xFF)*257;
	canvas->backColor.blue  = ((bcolor >> 16) & 0xFF)*257;

	if (canvas->drawable)
	{
		canvas->theDrawGC = gdk_gc_new(canvas->drawable);
		gdk_gc_set_clip_origin(canvas->theDrawGC, 0, 0);

		canvas->theFillGC = gdk_gc_new(canvas->drawable);
		gdk_gc_set_clip_origin(canvas->theFillGC, 0, 0);

		canvas->theEraseGC = gdk_gc_new(canvas->drawable);
		gdk_gc_set_clip_origin(canvas->theEraseGC, 0, 0);

		canvas->theTextGC = gdk_gc_new(canvas->drawable);
		gdk_gc_set_clip_origin(canvas->theTextGC, 0, 0);

		if (canvas->region)
		{
			gdk_gc_set_clip_region(canvas->theDrawGC, canvas->region);
			gdk_gc_set_clip_region(canvas->theFillGC, canvas->region);
			gdk_gc_set_clip_region(canvas->theEraseGC,canvas->region);
			gdk_gc_set_clip_region(canvas->theTextGC, canvas->region);
		}
	}
	else
	{
		canvas->theDrawGC = NULL;
		canvas->theFillGC = NULL;
		canvas->theEraseGC= NULL;
		canvas->theTextGC = NULL;
	}

	canvas->theFont = font;
	osChangeCanvasPen(size,function,pcolor,bcolor,joinStyle,capStyle,lineStyle,lineCustomCount,lineCustomDashes,backDraw,hatchStyle,hatchBitmap,font,canvas);

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
		gdk_draw_rectangle(canvas->drawable, canvas->theEraseGC, TRUE,
				clip_box.x, clip_box.y, clip_box.width, clip_box.height);
	}
}	/* osInitCanvas */

void osDoneCanvas (CanvasHandle canvas)
{
	if (canvas->buffered == 1)
	{
		gdk_window_end_paint(GDK_WINDOW(canvas->drawable));
		canvas->buffered = 0;
	}

	if (canvas->drawable)
	{
		gdk_colormap_free_colors(gdk_drawable_get_colormap(canvas->drawable), &canvas->penColor, 1);
		gdk_colormap_free_colors(gdk_drawable_get_colormap(canvas->drawable), &canvas->backColor, 1);
	}

	if (canvas->tile)
	{
		gdk_pixmap_unref(canvas->tile);
		canvas->tile = NULL;
	}

	if (canvas->theDrawGC)    gdk_gc_destroy(canvas->theDrawGC);
	if (canvas->theFillGC)    gdk_gc_destroy(canvas->theFillGC);
	if (canvas->theEraseGC)   gdk_gc_destroy(canvas->theEraseGC);
	if (canvas->theTextGC)    gdk_gc_destroy(canvas->theTextGC);

	if (canvas->lineCustomDashes)
		free(canvas->lineCustomDashes);
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

static gint8 dashStyles[] = {15,15};
static gint8 dotStyles[] = {1,4};
static gint8 dashDotStyles[] = {5,3,1,3};
static gint8 dashDotDotStyles[] = {5,3,1,3,1,3};

gchar bdiag_xbm[] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};
gchar fdiag_xbm[] = {0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x01};
gchar cross_xbm[] = {0xFF,0x01,0x01,0x01,0x01,0x01,0x01,0x01};
gchar dcross_xbm[]= {0x81,0x42,0x24,0x18,0x18,0x24,0x42,0x81};
gchar horiz_xbm[] = {0xFF,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
gchar vert_xbm[]  = {0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01};

void osChangeCanvasPen(int size, int function,
					 unsigned int pcolor,
					 unsigned int bcolor,
					 int joinStyle,
					 int capStyle,
					 int lineStyle,
					 int lineCustomCount,
					 unsigned char *lineCustomDashes,
					 BOOL backDraw,
					 int hatchStyle,
					 BitmapHandle hatchBitmap,
					 FontHandle font,
					 CanvasHandle canvas
					 )
{
	GdkCapStyle  gtkCapStyle;
	GdkJoinStyle gtkJoinStyle;
	GdkLineStyle gtkLineStyle;
	GdkFill fill;
	GdkColormap *colormap;
	GdkColor blackColor, whiteColor;
	GdkGC *gc;

	if (canvas->drawable)
	{
		if (canvas->lineCustomDashes)
			free(canvas->lineCustomDashes);

		canvas->lineCustomCount = lineCustomCount;
		canvas->lineCustomDashes = lineCustomDashes;
		canvas->backDraw = backDraw;

		switch (joinStyle)
		{
		case 0: gtkJoinStyle = GDK_JOIN_BEVEL; break;
		case 1: gtkJoinStyle = GDK_JOIN_MITER; break;
		case 2: gtkJoinStyle = GDK_JOIN_ROUND; break;
		}

		switch (capStyle)
		{
		case 0: gtkCapStyle = GDK_CAP_ROUND;  	  break;
		case 1: gtkCapStyle = GDK_CAP_PROJECTING; break;
		case 2: gtkCapStyle = GDK_CAP_BUTT;   	  break;
		}

		if (lineStyle == 0)
			gtkLineStyle = GDK_LINE_SOLID;
		else
			if (canvas->backDraw)
				gtkLineStyle = GDK_LINE_DOUBLE_DASH;
			else
				gtkLineStyle = GDK_LINE_ON_OFF_DASH;

		gdk_gc_set_line_attributes(canvas->theDrawGC,   size, gtkLineStyle, gtkCapStyle, gtkJoinStyle);
		gdk_gc_set_line_attributes(canvas->theFillGC,   size, gtkLineStyle, gtkCapStyle, gtkJoinStyle);
		gdk_gc_set_line_attributes(canvas->theEraseGC,  size, gtkLineStyle, gtkCapStyle, gtkJoinStyle);

		switch (lineStyle)
		{
		case 1:
			gdk_gc_set_dashes(canvas->theDrawGC, 0,dashStyles,G_N_ELEMENTS(dashStyles));
			gdk_gc_set_dashes(canvas->theFillGC, 0,dashStyles,G_N_ELEMENTS(dashStyles));
			gdk_gc_set_dashes(canvas->theEraseGC,0,dashStyles,G_N_ELEMENTS(dashStyles));
			break;
		case 2:
			gdk_gc_set_dashes(canvas->theDrawGC, 0,dotStyles,G_N_ELEMENTS(dotStyles));
			gdk_gc_set_dashes(canvas->theFillGC, 0,dotStyles,G_N_ELEMENTS(dotStyles));
			gdk_gc_set_dashes(canvas->theEraseGC,0,dotStyles,G_N_ELEMENTS(dotStyles));
			break;
		case 3:
			gdk_gc_set_dashes(canvas->theDrawGC, 0,dashDotStyles,G_N_ELEMENTS(dashDotStyles));
			gdk_gc_set_dashes(canvas->theFillGC, 0,dashDotStyles,G_N_ELEMENTS(dashDotStyles));
			gdk_gc_set_dashes(canvas->theEraseGC,0,dashDotStyles,G_N_ELEMENTS(dashDotStyles));
			break;
		case 4:
			gdk_gc_set_dashes(canvas->theDrawGC, 0,dashDotDotStyles,G_N_ELEMENTS(dashDotDotStyles));
			gdk_gc_set_dashes(canvas->theFillGC, 0,dashDotDotStyles,G_N_ELEMENTS(dashDotDotStyles));
			gdk_gc_set_dashes(canvas->theEraseGC,0,dashDotDotStyles,G_N_ELEMENTS(dashDotDotStyles));
			break;
		case 5:
			gdk_gc_set_dashes(canvas->theDrawGC, 0,canvas->lineCustomDashes,canvas->lineCustomCount);
			gdk_gc_set_dashes(canvas->theFillGC, 0,canvas->lineCustomDashes,canvas->lineCustomCount);
			gdk_gc_set_dashes(canvas->theEraseGC,0,canvas->lineCustomDashes,canvas->lineCustomCount);
			break;
		}

		colormap = gdk_drawable_get_colormap(canvas->drawable);

		if (canvas->penColor.pixel) gdk_colormap_free_colors(colormap, &canvas->penColor, 1);
		canvas->penColor.pixel = 0;
		canvas->penColor.red   = ((pcolor      ) & 0xFF)*257;
		canvas->penColor.green = ((pcolor >>  8) & 0xFF)*257;
		canvas->penColor.blue  = ((pcolor >> 16) & 0xFF)*257;
		gdk_colormap_alloc_color(colormap, &canvas->penColor, FALSE, FALSE);

		gdk_gc_set_foreground(canvas->theDrawGC, &canvas->penColor);
		gdk_gc_set_foreground(canvas->theFillGC, &canvas->penColor);
		gdk_gc_set_background(canvas->theEraseGC, &canvas->penColor);
		gdk_gc_set_foreground(canvas->theTextGC, &canvas->penColor);

		if (canvas->backColor.pixel) gdk_colormap_free_colors(gdk_drawable_get_colormap(canvas->drawable), &canvas->backColor, 1);
		canvas->backColor.pixel = 0;
		canvas->backColor.red   = ((bcolor      ) & 0xFF)*257;
		canvas->backColor.green = ((bcolor >>  8) & 0xFF)*257;
		canvas->backColor.blue  = ((bcolor >> 16) & 0xFF)*257;
		gdk_colormap_alloc_color(gdk_drawable_get_colormap(canvas->drawable), &canvas->backColor, FALSE, FALSE);

		gdk_gc_set_background(canvas->theDrawGC, &canvas->backColor);
		gdk_gc_set_background(canvas->theFillGC, &canvas->backColor);
		gdk_gc_set_foreground(canvas->theEraseGC,&canvas->backColor);
		gdk_gc_set_background(canvas->theTextGC, &canvas->backColor);

		switch (function)
		{
			case 0:
				gdk_gc_set_function(canvas->theDrawGC,   GDK_COPY);
				gdk_gc_set_function(canvas->theFillGC,   GDK_COPY);
				gdk_gc_set_function(canvas->theEraseGC,  GDK_COPY);
				gdk_gc_set_function(canvas->theTextGC,   GDK_COPY);
				break;
			case 1:
				gdk_gc_set_function(canvas->theDrawGC,   GDK_INVERT);
				gdk_gc_set_function(canvas->theFillGC,   GDK_INVERT);
				gdk_gc_set_function(canvas->theEraseGC,  GDK_INVERT);
				gdk_gc_set_function(canvas->theTextGC,   GDK_INVERT);
				break;
			case 2:
				gdk_gc_set_function(canvas->theDrawGC,   GDK_XOR);
				gdk_gc_set_function(canvas->theFillGC,   GDK_XOR);
				gdk_gc_set_function(canvas->theEraseGC,  GDK_XOR);
				gdk_gc_set_function(canvas->theTextGC,   GDK_XOR);
				break;
		}

		if (canvas->tile)
		{
			gdk_pixmap_unref(canvas->tile);
			canvas->tile = NULL;
		}

		switch (hatchStyle)
		{
		case 0:
			fill = GDK_SOLID;
			break;
		case 7:
			fill = GDK_TILED;
			break;
		default:
			fill = (canvas->backDraw) ? GDK_OPAQUE_STIPPLED : GDK_STIPPLED;
			break;
		}

		gdk_gc_set_fill(canvas->theFillGC,   fill);

		gdk_gc_set_fill(canvas->theEraseGC,
			(fill == GDK_OPAQUE_STIPPLED) ? GDK_STIPPLED : fill);

		if (size > 1)
			gdk_gc_set_fill(canvas->theDrawGC,  fill);

		whiteColor.pixel = 0;
		whiteColor.red   = 65535;
		whiteColor.green = 65535;
		whiteColor.blue  = 65535;
		gdk_colormap_alloc_color(colormap, &whiteColor, FALSE, FALSE);
		blackColor.pixel = 0;
		blackColor.red   = 0;
		blackColor.green = 0;
		blackColor.blue  = 0;
		gdk_colormap_alloc_color(colormap, &blackColor, FALSE, FALSE);

		switch (hatchStyle)
		{
		case 1:
			canvas->tile = gdk_pixmap_create_from_data(NULL, bdiag_xbm, 8, 8, 1, &whiteColor, &blackColor);
			gdk_gc_set_stipple(canvas->theFillGC,   canvas->tile);
			gdk_gc_set_stipple(canvas->theEraseGC,  canvas->tile);

			if (size > 1)
				gdk_gc_set_stipple(canvas->theDrawGC,  canvas->tile);
			break;
		case 2:
			canvas->tile = gdk_pixmap_create_from_data(NULL, fdiag_xbm, 8, 8, 1, &whiteColor, &blackColor);
			gdk_gc_set_stipple(canvas->theFillGC,   canvas->tile);
			gdk_gc_set_stipple(canvas->theEraseGC,  canvas->tile);

			if (size > 1)
				gdk_gc_set_stipple(canvas->theDrawGC,  canvas->tile);
			break;
		case 3:
			canvas->tile = gdk_pixmap_create_from_data(NULL, cross_xbm, 8, 8, 1, &whiteColor, &blackColor);
			gdk_gc_set_stipple(canvas->theFillGC,   canvas->tile);
			gdk_gc_set_stipple(canvas->theEraseGC,  canvas->tile);

			if (size > 1)
				gdk_gc_set_stipple(canvas->theDrawGC,  canvas->tile);
			break;
		case 4:
			canvas->tile = gdk_pixmap_create_from_data(NULL, dcross_xbm, 8, 8, 1, &whiteColor, &blackColor);
			gdk_gc_set_stipple(canvas->theFillGC,   canvas->tile);
			gdk_gc_set_stipple(canvas->theEraseGC,  canvas->tile);

			if (size > 1)
				gdk_gc_set_stipple(canvas->theDrawGC,  canvas->tile);
			break;
		case 5:
			canvas->tile = gdk_pixmap_create_from_data(NULL, horiz_xbm, 8, 8, 1, &whiteColor, &blackColor);
			gdk_gc_set_stipple(canvas->theFillGC,   canvas->tile);
			gdk_gc_set_stipple(canvas->theEraseGC,  canvas->tile);

			if (size > 1)
				gdk_gc_set_stipple(canvas->theDrawGC,  canvas->tile);
			break;
		case 6:
			canvas->tile = gdk_pixmap_create_from_data(NULL, vert_xbm, 8, 8, 1, &whiteColor, &blackColor);
			gdk_gc_set_stipple(canvas->theFillGC,   canvas->tile);
			gdk_gc_set_stipple(canvas->theEraseGC,  canvas->tile);

			if (size > 1)
				gdk_gc_set_stipple(canvas->theDrawGC,  canvas->tile);
			break;
		case 7:
			{
				canvas->tile = gdk_pixmap_new(NULL, hatchBitmap->width, hatchBitmap->height, colormap->visual->depth);

				gdk_drawable_set_colormap(GDK_DRAWABLE(canvas->tile), colormap);

				gc = gdk_gc_new(GDK_DRAWABLE(canvas->tile));

				gdk_pixbuf_render_to_drawable(hatchBitmap->pixbuf, GDK_DRAWABLE(canvas->tile), gc,
					0, 0, 0, 0, hatchBitmap->width, hatchBitmap->height,
		    	GDK_RGB_DITHER_NONE, 0, 0);

				gdk_gc_destroy(gc);

				gdk_gc_set_tile(canvas->theFillGC,  canvas->tile);
				gdk_gc_set_tile(canvas->theEraseGC, canvas->tile);

				if (size > 1)
					gdk_gc_set_tile(canvas->theDrawGC,  canvas->tile);
			}
			break;
		}

		gdk_colormap_free_colors(colormap, &whiteColor, 1);
		gdk_colormap_free_colors(colormap, &blackColor, 1);

		canvas->theFont = font;
		pango_layout_set_font_description(canvas->layout, font->font_descr);
	}
}

void osRotateCanvas(double angle, CanvasHandle canvas)
{
	printf("osRotateCanvas -> not implemented\n");
};

void osScaleCanvas(double dScaleX, double dScaleY, CanvasHandle canvas)
{
	printf("osScaleCanvas -> not implemented\n");
};

void osShearCanvas(double dShearX, double dShearY, CanvasHandle canvas)
{
	printf("osShearCanvas -> not implemented\n");
};

void osTranslateCanvas(double dDeltaX, double dDeltaY, CanvasHandle canvas)
{
	printf("osTranslateCanvas -> not implemented\n");
};

void osDrawPoint(int x, int y, CanvasHandle canvas)
{
	if (canvas->drawable) gdk_draw_point(canvas->drawable, canvas->theDrawGC, x, y);
}	/* osDrawPoint */

void osDrawLine(int startx, int starty, int endx, int endy, CanvasHandle canvas)
{
	if (canvas->drawable) gdk_draw_line(canvas->drawable, canvas->theDrawGC, startx, starty, endx, endy);
}	/* osDrawLine */

static float PI = 3.1415926535897932384626433832795;

static void osGtkCurve(GdkDrawable* drawable, GdkGC* gc, gboolean fill, int x0, int y0, int x1, int y1, float from, float to, gboolean clockwise)
{
  if (drawable) {
    int dist;

    /* normalise angles */
    from = fmod(from,2*PI); if (from < 0) from = 2*PI + from;
    to   = fmod(to,2*PI);   if (to < 0)   to = 2*PI + to;

    /* convert to gtk angles */
    from = (32*360*from)/PI;
    to   = (32*360*to)/PI;

    dist = floor(clockwise ? to-from : from-to);
    gdk_draw_arc( drawable, gc, fill, x0, y0, (x1-x0), (y1-y0), from, dist );
  }
}

void osDrawCurve(int x0, int y0, int x1, int y1, float from, float to, gboolean clockwise,CanvasHandle canvas)
{
  osGtkCurve( canvas->drawable, canvas->theDrawGC, FALSE, x0, y0, x1, y1, from, to, clockwise );
}

void osFillCurve(int x0, int y0, int x1, int y1, float from, float to, gboolean clockwise,CanvasHandle canvas)
{
  osGtkCurve( canvas->drawable, canvas->theFillGC, TRUE, x0, y0, x1, y1, from, to, clockwise );
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
		}

		if (canvas->theFont->style & FONT_UNDERLINED)
		{
			attr = pango_attr_underline_new(PANGO_UNDERLINE_SINGLE);
			attr->start_index = 0;
			attr->end_index = len;

			pango_attr_list_change(attr_list, attr);
		}

		if (canvas->theFont->style & FONT_STRIKED)
		{
			attr = pango_attr_strikethrough_new(gtk_true());
			attr->start_index = 0;
			attr->end_index = len;

			pango_attr_list_change(attr_list, attr);
		}
	}
}

void osDrawChar (int x, int y, char c, CanvasHandle canvas)
{
	if (canvas->drawable)
	{
		PangoLayoutLine *layout_line;

		osSetupFont(canvas, 1);
		pango_layout_set_text(canvas->layout, &c, 1);
		layout_line = pango_layout_get_line(canvas->layout, 0);

		gdk_draw_layout_line_with_colors (canvas->drawable,
                        canvas->theTextGC,
                        x, y,
                        layout_line,
                        NULL, canvas->backDraw ? &canvas->backColor : NULL);

		pango_layout_set_text(canvas->layout, NULL, 0);
	}
}	/* osDrawChar */

void osDrawString (int x, int y, char *string, CanvasHandle canvas)
{
	if (canvas->drawable)
	{
		PangoLayoutLine *layout_line;

		osSetupFont(canvas, strlen(string));
		pango_layout_set_text(canvas->layout, string, -1);
		layout_line = pango_layout_get_line(canvas->layout, 0);

		gdk_draw_layout_line_with_colors (canvas->drawable,
                        canvas->theTextGC,
                        x, y,
                        layout_line,
                        NULL, canvas->backDraw ? &canvas->backColor : NULL);

		pango_layout_set_text(canvas->layout, NULL, 0);
	}
}	/* osDrawString */

void osDrawRect(int left, int top, int right, int bot, CanvasHandle canvas)
{
	if (canvas->drawable)
		gdk_draw_rectangle(canvas->drawable, canvas->theDrawGC, FALSE,
	                   left, top,
	                   right-left,
	                   bot-top);
}	/* osDrawRect */

void osFillRect(int left, int top, int right, int bot, CanvasHandle canvas)
{
	if (canvas->drawable)
		gdk_draw_rectangle(canvas->drawable, canvas->theFillGC, TRUE,
	                   left, top,
	                   right-left+1,
	                   bot-top+1);
}	/* osFillRect */

void osDrawOval (int left, int top, int right, int bot, CanvasHandle canvas)
{
	if (canvas->drawable) gdk_draw_arc(canvas->drawable,canvas->theDrawGC,FALSE,left,top,right-left,bot-top,0,64*360);
}	/* osDrawOval */

void osFillOval (int left, int top, int right, int bot, CanvasHandle canvas)
{
	if (canvas->drawable) gdk_draw_arc(canvas->drawable,canvas->theFillGC,TRUE,left,top,right-left,bot-top,0,64*360);
}	/* osFillOval */

void osDrawPolyline(PolygonHandle arr, CanvasHandle canvas)
{
	if (canvas->drawable) gdk_draw_lines(canvas->drawable,canvas->theDrawGC,arr->data,arr->count);
}	/* osDrawPolygon */


void osDrawPolygon(PolygonHandle arr, CanvasHandle canvas)
{
	if (canvas->drawable) gdk_draw_polygon(canvas->drawable,canvas->theDrawGC,FALSE,arr->data,arr->count);
}	/* osDrawPolygon */


void osFillPolygon (PolygonHandle arr, CanvasHandle canvas)
{
	if (canvas->drawable) gdk_draw_polygon(canvas->drawable,canvas->theFillGC,TRUE,arr->data,arr->count);
}	/* osFillPolygon */

CanvasHandle osGetTemporaryCanvas()
{
	Display *display;
	CanvasHandle canvas;
	PangoContext *pango_context;

	display = GDK_DISPLAY_XDISPLAY(gdk_display_get_default());
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
	if (canvas->drawable)
    {
		gdk_pixbuf_render_to_drawable(bmp->pixbuf, canvas->drawable, canvas->theDrawGC, 0, 0, destx, desty,
      										bmp->width, bmp->height,
                                 			GDK_RGB_DITHER_NONE, 0, 0);
    }
}	/* osDrawBitmap */

unsigned int gdk_color_to_rgb(GdkColor color)
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
