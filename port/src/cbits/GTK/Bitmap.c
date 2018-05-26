#include "Bitmap.h"
#include "Internals.h"

#define MIME_PREFIX "image/"

BitmapHandle osReadBitmap(char *filename, int *pRes)
{
	GError *err = NULL;
	BitmapHandle bitmap;
	GdkPixbuf *pixbuf;

	pixbuf = gdk_pixbuf_new_from_file(filename, &err);

	if (!pixbuf)
	{
		*pRes = 1;

		if (err->domain == GDK_PIXBUF_ERROR)
		{
			switch (err->code)
			{
			case GDK_PIXBUF_ERROR_CORRUPT_IMAGE:
			case GDK_PIXBUF_ERROR_UNKNOWN_TYPE:
			case GDK_PIXBUF_ERROR_UNSUPPORTED_OPERATION:
			case GDK_PIXBUF_ERROR_FAILED:
				*pRes = 2;
				break;
			}
		}
		else
			if (err->domain == G_FILE_ERROR)
			{
				switch (err->code)
				{
				case G_FILE_ERROR_ISDIR:
				case G_FILE_ERROR_ACCES:
				case G_FILE_ERROR_PERM:
					*pRes = 4;
					break;
				case G_FILE_ERROR_NOENT:
					*pRes = 3;
					break;
				}
			}

		return NULL;
	}

	bitmap = rmalloc(sizeof(*bitmap));
	bitmap->pixbuf = pixbuf;
	bitmap->width  = gdk_pixbuf_get_width(pixbuf);
	bitmap->height = gdk_pixbuf_get_height(pixbuf);

	*pRes = 0;
	return bitmap;
}	/* osCreateBitmap */

BitmapHandle osCreateBitmap(int width, int height)
{
	BitmapHandle bitmap = rmalloc(sizeof(*bitmap));
	bitmap->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB,gtk_false(),8,width,height);
	bitmap->width  = width;
	bitmap->height = height;
	return bitmap;
}

void osSetBitmapSize (BitmapHandle bitmap, int width, int height)
{
	GdkPixbuf *new_pixbuf;

	new_pixbuf = gdk_pixbuf_scale_simple(bitmap->pixbuf, width, height, GDK_INTERP_BILINEAR);

	if (new_pixbuf)
	{
		g_object_unref(bitmap->pixbuf);

		bitmap->pixbuf = new_pixbuf;
		bitmap->width  = width;
		bitmap->height = height;
	}
}

void osGetBitmapSize (BitmapHandle bitmap, int *size)
{
	size[0] = bitmap->width;
	size[1] = bitmap->height;
}

void osDeleteBitmap (BitmapHandle bitmap)
{
	g_object_unref(bitmap->pixbuf);
	rfree(bitmap);
}

CanvasHandle osGetBitmapCanvas(BitmapHandle bitmap)
{
	Display *display;
	CanvasHandle canvas;
	GdkRectangle rectangle;
	GdkColormap *sys_colormap;
	PangoContext *pango_context;

     /* Draw the pixbuf */
	rectangle.x = 0;
	rectangle.y = 0;
	rectangle.width  = bitmap->width;
	rectangle.height = bitmap->height;

	sys_colormap = gdk_colormap_get_system();
	
	GdkDrawable *drawable = GDK_DRAWABLE(gdk_pixmap_new(NULL, bitmap->width, bitmap->height, sys_colormap->visual->depth));

	pango_context = pango_context_new();
	gdk_pango_context_set_colormap(pango_context, sys_colormap);

	canvas = rmalloc(sizeof(*canvas));
	memset(canvas, 0, sizeof(*canvas));
	canvas->drawable= drawable;
	canvas->cr      = gdk_cairo_create (drawable);
	canvas->layout  = pango_layout_new(pango_context);
	canvas->region  = gdk_region_rectangle(&rectangle);
	canvas->buffered= 0;
	canvas->pixbuf  = bitmap->pixbuf;

	gdk_drawable_set_colormap(canvas->drawable, sys_colormap);

	gdk_cairo_set_source_pixbuf (canvas->cr, bitmap->pixbuf, 0, 0);
	cairo_paint (canvas->cr);

	return canvas;
}

void osReleaseBitmapCanvas(CanvasHandle canvas)
{
	cairo_destroy(canvas->cr);

	gdk_pixbuf_get_from_drawable(canvas->pixbuf,canvas->drawable,
                               NULL,0,0,0,0,-1,-1);

	gdk_pixmap_unref(canvas->drawable);
	if (canvas->region) gdk_region_destroy(canvas->region);
	g_object_unref(canvas->layout);
}

int osWriteBitmap(BitmapHandle bitmap, char *format, char *fname)
{
	GError *err = NULL;
	int len = strlen(MIME_PREFIX);

	if (strlen(format) < len)
		return 2;

	if (!gdk_pixbuf_save(bitmap->pixbuf, fname, format + len, &err, NULL))
	{
		if (err->domain == GDK_PIXBUF_ERROR)
		{
			switch (err->code)
			{
			case GDK_PIXBUF_ERROR_CORRUPT_IMAGE:
			case GDK_PIXBUF_ERROR_UNKNOWN_TYPE:
			case GDK_PIXBUF_ERROR_UNSUPPORTED_OPERATION:
			case GDK_PIXBUF_ERROR_FAILED:
				return 2;
			}
		}
		else
			if (err->domain == G_FILE_ERROR)
			{
				switch (err->code)
				{
				case G_FILE_ERROR_ISDIR:
				case G_FILE_ERROR_ACCES:
				case G_FILE_ERROR_PERM:
					return 3;
				}
			}

		return 1;
	}

	return 0;
}

static gboolean gdk_save_callback(const gchar *buf, gsize count, GError **error, gpointer data)
{
	int fh = *((int*) data);
	return write(fh, buf, count) == count;
}

int osWriteBitmapHandle(BitmapHandle bitmap, char *format, int fhandle)
{
	GError *err = NULL;
	int len = strlen(MIME_PREFIX);

	if (strlen(format) < len)
		return 2;

	if (!gdk_pixbuf_save_to_callback(bitmap->pixbuf, gdk_save_callback, &fhandle, format + len, &err, NULL))
	{
		if (err->domain == GDK_PIXBUF_ERROR)
		{
			switch (err->code)
			{
			case GDK_PIXBUF_ERROR_CORRUPT_IMAGE:
			case GDK_PIXBUF_ERROR_UNKNOWN_TYPE:
			case GDK_PIXBUF_ERROR_UNSUPPORTED_OPERATION:
			case GDK_PIXBUF_ERROR_FAILED:
				return 2;
			}
		}
		else
			if (err->domain == G_FILE_ERROR)
			{
				switch (err->code)
				{
				case G_FILE_ERROR_ISDIR:
				case G_FILE_ERROR_ACCES:
				case G_FILE_ERROR_PERM:
					return 3;
				}
			}

		return 1;
	}

	return 0;
}

void *osInitEncodersEnumerator()
{
	CodecsEnumeratorHandle enumerator;

	enumerator = rmalloc(sizeof(*enumerator));
	enumerator->hasCurrent = FALSE;
	enumerator->first = gdk_pixbuf_get_formats();
	enumerator->current = NULL;
	return enumerator;
}

BOOL osSelectNextEncoder(CodecsEnumeratorHandle enumerator)
{
	if (enumerator)
	{
		if (enumerator->hasCurrent)
		{
			if (enumerator->current)
			{
				enumerator->current = enumerator->current->next;
				return (enumerator->current != NULL);
			}
		}
		else
		{
			enumerator->current = enumerator->first;
			enumerator->hasCurrent = TRUE;
			return TRUE;
		}
	}

	return FALSE;
}

char *osGetCurrentEncoderName(CodecsEnumeratorHandle enumerator)
{
	if (enumerator && enumerator->current)
	{
		char *s = gdk_pixbuf_format_get_name((GdkPixbufFormat *) enumerator->current->data);
		char *name = rmalloc(strlen(s)+1);

		strcpy(name,s);
		return name;
	}

	return NULL;
};

char *osGetCurrentEncoderDescription(CodecsEnumeratorHandle enumerator)
{
	if (enumerator && enumerator->current)
	{
		char *s = gdk_pixbuf_format_get_description((GdkPixbufFormat *) enumerator->current->data);
		char *name = rmalloc(strlen(s)+1);

		strcpy(name,s);
		return name;
	}

	return NULL;
};

char *osGetCurrentEncoderMime(CodecsEnumeratorHandle enumerator)
{
	if (enumerator && enumerator->current)
	{
		char *s = gdk_pixbuf_format_get_name((GdkPixbufFormat *) enumerator->current->data);
		char *mime = rmalloc(strlen(MIME_PREFIX)+strlen(s)+1);

		strcpy(mime,MIME_PREFIX);
		strcat(mime,s);
		return mime;
	}

	return NULL;
};

BOOL osGetCurrentEncoderReadable(CodecsEnumeratorHandle enumerator)
{
	return TRUE;
};

BOOL osGetCurrentEncoderWritable(CodecsEnumeratorHandle enumerator)
{
	if (enumerator && enumerator->current)
	{
		return gdk_pixbuf_format_is_writable((GdkPixbufFormat *) enumerator->current->data);
	}

	return FALSE;
};

char *osGetCurrentEncoderExtensions(CodecsEnumeratorHandle enumerator)
{
	if (enumerator && enumerator->current)
	{
		int count;
		char *r, *res;
		char **a, **arr = gdk_pixbuf_format_get_extensions((GdkPixbufFormat *) enumerator->current->data);

		count = 0;
		for (a = arr; *a; a++)
			count += strlen(*a)+1;

		res = rmalloc(count+1);

		r = res;
		for (a = arr; *a; a++)
		{
			strcpy(r,*a);
			r += strlen(*a)+1;
		}
		*r = 0;

		return res;
	}

	return NULL;
};

void osFreeEncodersEnumerator(CodecsEnumeratorHandle enumerator)
{
	if (enumerator && enumerator->first)
	{
		g_slist_free(enumerator->first);
	}
}
