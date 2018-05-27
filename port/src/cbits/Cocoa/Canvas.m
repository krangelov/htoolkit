#include "Types.h"
#include "Canvas.h"
#include "Font.h"
#include "Internals.h"

int osMMtoVPixels(double mm)
{
	printf("osMMtoVPixels\n");
	return 0;
}

int osMMtoHPixels(double mm)
{
	printf("osMMtoHPixels\n");
	return 0;
}

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
					 void* fill_info,
					 FontHandle font,
					 CanvasHandle canvas,
					 BOOL buffered
					)
{
	printf("osInitCanvas\n");
}	/* osInitCanvas */

void osDoneCanvas (CanvasHandle canvas)
{
	printf("osDoneCanvas\n");
}	/* osDoneCanvas */

/*	Operations to create, modify, and destroy polygon shapes.
*/

PolygonHandle osCreatePolygon(int size)
{
	printf("osCreatePolygon\n");
	return NULL;
}	/* osCreatePolygon */

void osAddPolygonPoint(PolygonHandle shape, int x, int y)
{
	printf("osAddPolygonPoint\n");
}	/* osAddPolygonPoint */

void osDeletePolygon(PolygonHandle shape)
{
	printf("osDeletePolygon\n");
}	/* osDeletePolygon */


/*------------------------------------*\

|	   Interface functions			   |
\*------------------------------------*/

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
					 void* fill_info,
					 FontHandle font,
					 CanvasHandle canvas
					 )
{
	printf("osChangeCanvasPen\n");
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
	printf("osDrawPoint\n");
}	/* osDrawPoint */

void osDrawLine(int startx, int starty, int endx, int endy, CanvasHandle canvas)
{
	printf("osDrawLine\n");
}	/* osDrawLine */

void osDrawCurve(int x0, int y0, int x1, int y1, float from, float to, BOOL clockwise,CanvasHandle canvas)
{
  printf("osDrawCurve\n");
}

void osFillCurve(int x0, int y0, int x1, int y1, float from, float to, BOOL clockwise,CanvasHandle canvas)
{
  printf("osFillCurve\n");
}

static void osSetupFont(CanvasHandle canvas, int len)
{
	printf("osSetupFont\n");
}

void osDrawChar(int x, int y, char c, CanvasHandle canvas)
{
	printf("osDrawChar\n");
}	/* osDrawChar */

void osDrawString (int x, int y, char *string, CanvasHandle canvas)
{
	printf("osDrawString\n");
}	/* osDrawString */

void osDrawRect(int left, int top, int right, int bot, CanvasHandle canvas)
{
	printf("osDrawRect\n");
}	/* osDrawRect */

void osFillRect(int left, int top, int right, int bot, CanvasHandle canvas)
{
	printf("osFillRect\n");
}	/* osFillRect */

void osDrawOval (int left, int top, int right, int bot, CanvasHandle canvas)
{
	printf("osDrawOval\n");
}	/* osDrawOval */

void osFillOval (int left, int top, int right, int bot, CanvasHandle canvas)
{
	printf("osFillOval\n");
}	/* osFillOval */

void osDrawPolyline(PolygonHandle arr, CanvasHandle canvas)
{
	printf("osDrawPolyline\n");
}	/* osDrawPolyline */

void osDrawPolygon(PolygonHandle arr, CanvasHandle canvas)
{
	printf("osDrawPolygon\n");
}	/* osDrawPolygon */


void osFillPolygon (PolygonHandle arr, CanvasHandle canvas)
{
	printf("osFillPolygon\n");
}	/* osFillPolygon */

CanvasHandle osGetTemporaryCanvas()
{
	printf("osGetTemporaryCanvas\n");
	return NULL;
}	/* osGetTemporaryCanvas */

void osReleaseTemporaryCanvas (CanvasHandle canvas)
{
	printf("osReleaseTemporaryCanvas\n");
}	/* osReleaseTemporaryCanvas */

void osDrawBitmap(int destx, int desty, BitmapHandle bmp, CanvasHandle canvas)
{
	printf("osDrawBitmap\n");
}	/* osDrawBitmap */

unsigned int osGetDialogColor()
{
	printf("osGetDialogColor\n");
	return 0;
}

unsigned int osGetWindowColor()
{
	printf("osGetWindowColor\n");
	return 0;
}

unsigned int osGetTextColor()
{
	printf("osGetTextColor\n");
	return 0;
}
