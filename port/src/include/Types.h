#ifndef TYPES_H
#define TYPES_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <memory.h>
#include <string.h>
#include <time.h>

typedef enum {PosLeft=0, PosTop=1, PosRight=2, PosBottom=3} PositionType;

#ifdef WIN32_TARGET

#define _WIN32_IE 0x0501
#include <windows.h>
#include <mmsystem.h>
#include <shlobj.h>

#ifndef TB_GETBUTTONINFO
#define TB_GETBUTTONINFO (WM_USER + 65)
#endif

#ifndef I_IMAGENONE
#define I_IMAGENONE -2
#endif

typedef LPWSTR PortString;

typedef HWND WindowHandle;

typedef struct
{
	UINT_PTR id;
	int interval;
	BOOL enabled;
} *TimerHandle;

typedef struct
{
	HBITMAP hBitmap;
	SIZE sourcesize;
	SIZE destsize;
} *BitmapHandle;

typedef HFONT FontHandle;
typedef struct MenuHandle *MenuHandle;

typedef struct ToolHandle *ToolHandle;

typedef struct ActionHandle *ActionHandle;

typedef struct IndicatorHandle
{
	struct IndicatorHandle *next;
	struct IndicatorHandle *prev;
	char *title;
} *IndicatorHandle;

typedef struct
{
	int count, max_count;
	POINT data[0];
} *PolygonHandle;

typedef struct CanvasHandle
{
	HDC hDC;
	HDC hBufferedDC;
	HBITMAP hBufferBitmap;
	BOOL bInvalidated;
	HPEN thePen;
	HBRUSH theBrush;
	HFONT theFont;

	DWORD stylesCount;
	DWORD *stylesPtr;
} *CanvasHandle;

typedef void *CodecsEnumeratorHandle;

#elif GTK_TARGET

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <pango/pango.h>
#include <libgnome/libgnome.h>
#include <libgnomeui/libgnomeui.h>
#include <gconf/gconf.h>
#include <gconf/gconf-client.h>
#include <cairo.h>

typedef gchar* PortString;

typedef GtkWidget   *WindowHandle;

typedef struct
{
	guint id;
	int interval;
	gboolean enabled;
} *TimerHandle;

typedef struct
{
	GdkPixbuf *pixbuf;
	int width;
	int height;
} *BitmapHandle;

typedef cairo_pattern_t *GradientHandle;

typedef struct
{
	PangoFontDescription *font_descr;
	PangoFontMetrics *metrics;
	gint style;
} *FontHandle;

typedef GtkWidget *MenuHandle;

typedef GtkWidget *ToolHandle;

typedef struct ActionHandle *ActionHandle;

typedef GtkWidget *IndicatorHandle;

typedef struct _PortRecord* RowHandle;

typedef struct
{
	int count, max_count;
	GdkPoint data[0];
} *PolygonHandle;

typedef struct
{
	GdkDrawable *drawable;
	cairo_t *cr;
	FontHandle theFont;
	gboolean backDraw;
	unsigned int pcolor;
	unsigned int bcolor;
	GdkRegion *region;
	gboolean buffered;
	GdkPixbuf *pixbuf;
	guchar *tile;
	PangoLayout *layout;
} *CanvasHandle;

typedef gboolean BOOL;

typedef struct
{
	BOOL hasCurrent;
	GSList *first;
	GSList *current;
} *CodecsEnumeratorHandle;

#elif COCOA_TARGET

typedef char* PortString;

#include <AppKit/AppKit.h>

typedef NSView *WindowHandle;

typedef NSTimer *TimerHandle;

typedef NSImage *BitmapHandle;

typedef NSFont *FontHandle;

typedef NSMenuItem *MenuHandle;

typedef NSToolbarItem *ToolHandle;

@class NSActionHandle;

typedef NSActionHandle *ActionHandle;

typedef NSStatusItem *IndicatorHandle;

typedef void *PolygonHandle;

typedef void *CanvasHandle;

typedef struct CodecsEnumerator *CodecsEnumeratorHandle;

#else

#error Unsupported target

#endif

typedef void (OsInitFunc)();

void osStart(char *appTitle, char *appVersion, int DocumentInterface, OsInitFunc initFunc);
extern void osQuit();

#endif
