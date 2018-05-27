#include "Canvas.h"
#include "Internals.h"

int osMMtoVPixels(double mm)
{
	static int vres = 0;

	if (vres == 0)
	{
		HDC screen;
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
		if (screen==NULL)
		{
			printf("CreateDC returned NULL.\n");
			exit(1);
		}
		vres = GetDeviceCaps (screen, LOGPIXELSY);
		DeleteDC (screen);
	};

	return (int) ((mm/25.4) * vres);
}

int osMMtoHPixels(double mm)
{
	static int hres = 0;

	if (hres == 0)
	{
		HDC screen;
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
		hres = GetDeviceCaps (screen, LOGPIXELSX);
		DeleteDC (screen);
	};

	return (int) ((mm/25.4) * hres);
}

/*------------------------------------*\
|									   |
|	   Helper functions 			   |
|									   |
\*------------------------------------*/

void osInitCanvas (int size, int function,
					 unsigned int pcolor,
					 unsigned int bcolor,
					 int joinStyle,
					 int capStyle,
					 int lineStyle,
					 int stylesCount,
					 unsigned char *stylesPtr,
					 BOOL bkMode,
					 int fillStyle,
					 void* fill_info,
					 FontHandle font,
					 CanvasHandle canvas,
					 BOOL bDoubleBuffered
				    )
{
	RECT clipRect;

	canvas->stylesCount = 0;
	canvas->stylesPtr = NULL;
	canvas->thePen = NULL;
	canvas->theBrush = NULL;
	canvas->theFont = NULL;

	SaveDC(canvas->hDC);

	SetGraphicsMode(canvas->hDC, GM_ADVANCED);

	if (GetClipBox(canvas->hDC,&clipRect) == ERROR)
	{
		printf("osInitPicture -> GetClipBox failed\n");
		exit(1);
	}

	if (bDoubleBuffered)
	{
		canvas->hBufferedDC = canvas->hDC;
		canvas->hDC = CreateCompatibleDC(canvas->hBufferedDC);
		canvas->hBufferBitmap = CreateCompatibleBitmap(canvas->hBufferedDC,clipRect.right-clipRect.left,clipRect.bottom-clipRect.top);
		SelectObject(canvas->hDC, canvas->hBufferBitmap);

		if (!canvas->bInvalidated)
		{
			if (!BitBlt(canvas->hDC, 0, 0, clipRect.right-clipRect.left, clipRect.bottom-clipRect.top, canvas->hBufferedDC, clipRect.left, clipRect.top, SRCCOPY))
			{
				printf("osDonePicture -> BitBlt failed\n");
				exit(1);
			}
		}

		SetViewportOrgEx(canvas->hDC,-clipRect.left,-clipRect.top,NULL);
		SetBrushOrgEx(canvas->hDC,-clipRect.left,-clipRect.top,NULL);
	}

	SetPolyFillMode (canvas->hDC, WINDING);
	SetTextAlign (canvas->hDC, TA_LEFT | TA_BASELINE);
	SetStretchBltMode (canvas->hDC,COLORONCOLOR);

	osChangeCanvasPen(size,function,pcolor,bcolor,joinStyle,capStyle,lineStyle,stylesCount,stylesPtr,bkMode,fillStyle,fill_info,font,canvas);

	if (canvas->bInvalidated)
	{
		HBRUSH hb;
		LOGBRUSH lb;

		lb.lbColor = bcolor;
		SetupLogBrush(&lb, FALSE, fillStyle, fill_info);
		hb = CreateBrushIndirect(&lb);

		SetBkMode(canvas->hDC, OPAQUE);
		SetBkColor (canvas->hDC, pcolor);
		SelectObject (canvas->hDC, GetStockObject (NULL_PEN));
		SelectObject (canvas->hDC, hb);

		Rectangle(canvas->hDC, clipRect.left, clipRect.top, clipRect.right+1, clipRect.bottom+1);

		SelectObject (canvas->hDC, canvas->theBrush);
		SelectObject (canvas->hDC, canvas->thePen);
		if (!bkMode) SetBkMode(canvas->hDC, TRANSPARENT);

		DeleteObject(hb);
	}
}	/* osInitCanvas */

void osDoneCanvas (CanvasHandle canvas)
{
	if (canvas->hBufferedDC)
	{
		RECT clipRect;

		if (GetClipBox(canvas->hBufferedDC,&clipRect) == ERROR)
		{
			printf("osInitPicture -> GetClipBox failed\n");
			exit(1);
		}

		if (!BitBlt(canvas->hBufferedDC, clipRect.left, clipRect.top, clipRect.right-clipRect.left, clipRect.bottom-clipRect.top, canvas->hDC, clipRect.left, clipRect.top, SRCCOPY))
		{
			printf("osDonePicture -> BitBlt failed\n");
			exit(1);
		}

		DeleteDC(canvas->hDC);
		canvas->hDC = canvas->hBufferedDC;
		canvas->hBufferedDC = NULL;

		DeleteObject(canvas->hBufferBitmap);
		canvas->hBufferBitmap = NULL;
	}

	RestoreDC (canvas->hDC, -1);

	if (canvas->stylesPtr)
		free(canvas->stylesPtr);

	DeleteObject (canvas->thePen);
	DeleteObject (canvas->theBrush);
}	/* osDoneCanvas */

/*	Operations to create, modify, and destroy polygon shapes.
*/

PolygonHandle osCreatePolygon(int size)
{
	PolygonHandle polygon = (PolygonHandle) rmalloc (sizeof(*polygon) + size * sizeof (POINT));
	polygon->count = 0;
	polygon->max_count = size;
	return polygon;
}	/* osCreatePolygon */

void osAddPolygonPoint(PolygonHandle polygon, int x, int y)
{
	if (polygon->count > polygon->max_count)
	{
		printf("Too many points in PolygonHandle\n");
		exit(1);
	}

	polygon->data[polygon->count].x = x;
	polygon->data[polygon->count].y = y;
	polygon->count++;
}	/* osAddPolygonPoint */

void osDeletePolygon(PolygonHandle polygon)
{
	rfree (polygon);
}	/* osDeletePolygon */

void SetupLogBrush(LOGBRUSH *plb, BOOL bSetSolid, int hatchStyle, void* fill_info)
{
	switch (bSetSolid ? 0 : hatchStyle)
	{
	case 0:
		plb->lbStyle = BS_SOLID;
		plb->lbHatch = 0;
		break;
	case 1:
		plb->lbStyle = BS_HATCHED;
		plb->lbHatch = HS_BDIAGONAL;
		break;
	case 2:
		plb->lbStyle = BS_HATCHED;
		plb->lbHatch = HS_FDIAGONAL;
		break;
	case 3:
		plb->lbStyle = BS_HATCHED;
		plb->lbHatch = HS_CROSS;
		break;
	case 4:
		plb->lbStyle = BS_HATCHED;
		plb->lbHatch = HS_DIAGCROSS;
		break;
	case 5:
		plb->lbStyle = BS_HATCHED;
		plb->lbHatch = HS_HORIZONTAL;
		break;
	case 6:
		plb->lbStyle = BS_HATCHED;
		plb->lbHatch = HS_VERTICAL;
		break;
	case 9:
		plb->lbStyle = BS_PATTERN;
		plb->lbHatch = (LONG_PTR) ((BitmapHandle) fill_info)->hBitmap;
		break;
	}
}


/*------------------------------------*\
|	   Interface functions			   |
\*------------------------------------*/

void osChangeCanvasPen(int size, int function,
					 unsigned int pcolor,
					 unsigned int bcolor,
					 int joinStyle,
					 int capStyle,
					 int lineStyle,
					 int stylesCount,
					 unsigned char *stylesPtr,
					 BOOL bkMode,
					 int fillStyle,
					 void* fill_info,
					 FontHandle font,
					 CanvasHandle canvas
					 )
{
	HPEN hp;
	HBRUSH hb;
	LOGBRUSH lb;
	DWORD penstyle;
	int i;

	if (canvas->stylesPtr)
		free(canvas->stylesPtr);

	if (stylesPtr)
	{
		canvas->stylesCount = stylesCount;
		canvas->stylesPtr   = rmalloc(stylesCount*sizeof(int));

		for (i = 0; i < stylesCount; i++)
			canvas->stylesPtr[i] = stylesPtr[i];

		free(stylesPtr);
	}
	else
	{
		canvas->stylesCount = 0;
		canvas->stylesPtr   = NULL;
	}

	//if (penSize == 1 && lineStyle != PS_USERSTYLE)
		//penstyle = PS_COSMETIC;
	//else
		penstyle = PS_GEOMETRIC;

	switch (lineStyle)
	{
	case 0: penstyle |= PS_SOLID; break;
	case 1: penstyle |= PS_DASH; break;
	case 2: penstyle |= PS_DOT; break;
	case 3: penstyle |= PS_DASHDOT; break;
	case 4: penstyle |= PS_DASHDOTDOT; break;
	case 5: penstyle |= PS_USERSTYLE; break;
	}

	switch (joinStyle)
	{
	case 0: penstyle |= PS_JOIN_BEVEL; break;
	case 1: penstyle |= PS_JOIN_MITER; break;
	case 2: penstyle |= PS_JOIN_ROUND; break;
	}

	switch (capStyle)
	{
	case 0: penstyle |= PS_ENDCAP_ROUND;  break;
	case 1: penstyle |= PS_ENDCAP_SQUARE; break;
	case 2: penstyle |= PS_ENDCAP_FLAT;   break;
	}

	lb.lbColor = pcolor;
	SetupLogBrush(&lb, size == 1, fillStyle, fill_info);
	hp = ExtCreatePen (penstyle, size, &lb, canvas->stylesCount, canvas->stylesPtr);

	SelectObject (canvas->hDC, hp);
	if (canvas->thePen)
		DeleteObject (canvas->thePen);
	canvas->thePen = hp;

	lb.lbColor = pcolor;
	SetupLogBrush(&lb, FALSE, fillStyle, fill_info);
	hb = CreateBrushIndirect(&lb);

	SelectObject (canvas->hDC, hb);
	if (canvas->theBrush)
		DeleteObject(canvas->theBrush);
	canvas->theBrush = hb;

	switch (function)
	{
	case 0: SetROP2 (canvas->hDC, R2_COPYPEN); break;
	case 1:	SetROP2 (canvas->hDC, R2_NOT);     break;
	case 2: SetROP2 (canvas->hDC, R2_XORPEN);  break;
	}

	SetTextColor(canvas->hDC, pcolor);
	SetBkColor(canvas->hDC, bcolor);

	SetBkMode(canvas->hDC, bkMode ? OPAQUE : TRANSPARENT);

	SelectObject (canvas->hDC, font);
	canvas->theFont = font;
};

void osRotateCanvas(double angle, CanvasHandle canvas)
{
	XFORM xform;

	xform.eM22 =  (xform.eM11 = cos(angle));
  	xform.eM21 = -(xform.eM12 = sin(angle));
  	xform.eDx  = 0;
  	xform.eDy  = 0;
	ModifyWorldTransform(canvas->hDC, &xform, MWT_LEFTMULTIPLY);
}

void osScaleCanvas(double dScaleX, double dScaleY, CanvasHandle canvas)
{
	XFORM xform;

	xform.eM11 = dScaleX;
	xform.eM22 = dScaleY;
  	xform.eM21 = (xform.eM12 = 0);
  	xform.eDx  = 0;
  	xform.eDy  = 0;
	ModifyWorldTransform(canvas->hDC, &xform, MWT_LEFTMULTIPLY);
}

void osShearCanvas(double dShearX, double dShearY, CanvasHandle canvas)
{
	XFORM xform;

	xform.eM12 = dShearX;
	xform.eM21 = dShearY;
  	xform.eM11 = (xform.eM22 = 0);
  	xform.eDx  = 0;
  	xform.eDy  = 0;
	ModifyWorldTransform(canvas->hDC, &xform, MWT_LEFTMULTIPLY);
}

void osTranslateCanvas(double dDeltaX, double dDeltaY, CanvasHandle canvas)
{
	XFORM xform;

  	xform.eM11 = (xform.eM22 = 1);
  	xform.eM21 = (xform.eM12 = 0);
  	xform.eDx  = dDeltaX;
  	xform.eDy  = dDeltaY;
	ModifyWorldTransform(canvas->hDC, &xform, MWT_LEFTMULTIPLY);
}

void osDrawPoint (int x, int y, CanvasHandle canvas)
{
	SetPixelV (canvas->hDC, x, y, GetTextColor(canvas->hDC));
}	/* osDrawPoint */

void osDrawLine (int startx, int starty, int endx, int endy, CanvasHandle canvas)
{
	MoveToEx (canvas->hDC, startx, starty, NULL);
	LineTo (canvas->hDC, endx, endy);
}	/* osDrawLine */

void osDrawChar (int x, int y, char c, CanvasHandle canvas)
{
	TextOut (canvas->hDC, x, y, &c, 1);
}	/* osDrawChar */

void osDrawString (int x, int y, char *string, CanvasHandle canvas)
{
	TextOut (canvas->hDC, x, y, string, strlen(string));
}	/* osDrawString */

void osDrawRect(int left, int top, int right, int bot, CanvasHandle canvas)
{
	SelectObject (canvas->hDC, GetStockObject (NULL_BRUSH));
	Rectangle (canvas->hDC, left, top, right, bot);
	SelectObject (canvas->hDC, canvas->theBrush);
}	/* osDrawRect */

void osFillRect(int left, int top, int right, int bot, CanvasHandle canvas)
{
	SelectObject (canvas->hDC, GetStockObject (NULL_PEN));
	Rectangle(canvas->hDC, left, top, right+1, bot+1);
	SelectObject (canvas->hDC, canvas->thePen);
}	/* osFillRect */


static void osDrawFillCurve(BOOL fill, int x0, int y0, int x1, int y1, float from, float to, BOOL clockwise,CanvasHandle canvas)
{
	float rx,ry;
    float cx,cy;
    int   fx,fy;
    int   tx,ty;

    rx  = abs(x1-x0)/2;
    ry  = abs(y1-y0)/2;
    cx  = x0 + rx;
    cy  = y0 + ry;

	fx	= floor(cx + cos(from)*rx);
	fy	= floor(cy - sin(from)*ry);
	tx	= floor(cx + cos(to)*rx);
	ty	= floor(cy - sin(to)*ry);

    if (fill)
    {
      	if (clockwise)
      		Pie (canvas->hDC, x0, y0, x1, y1, tx, ty, fx, fy);
	    else
	    	Pie (canvas->hDC, x0, y0, x1, y1, fx, fy, tx, ty);
    }
    else
    {
		if (clockwise)
			Arc (canvas->hDC, x0, y0, x1, y1, tx, ty, fx, fy);
		else
			Arc (canvas->hDC, x0, y0, x1, y1, fx, fy, tx, ty);
    }
}	/* osDrawCurve */

void osDrawCurve (int x0, int y0, int x1, int y1, float from, float to, BOOL clockwise,CanvasHandle canvas)
{
    osDrawFillCurve(FALSE,x0,y0,x1,y1,from,to,clockwise,canvas);
}	/* osDrawCurve */

void osDrawOval (int left, int top, int right, int bot, CanvasHandle canvas)
{
	Arc (canvas->hDC, left, top, right, bot, 0, 0, 0, 0);
}	/* osDrawOval */

void osFillOval (int left, int top, int right, int bot, CanvasHandle canvas)
{
	Ellipse (canvas->hDC, left, top, right + 1, bot + 1);
}	/* osFillOval */

void osFillCurve(int x0, int y0, int x1, int y1, float from, float to, BOOL clockwise,CanvasHandle canvas)
{
    osDrawFillCurve(TRUE,x0,y0,x1,y1,from,to,clockwise,canvas);
} /* osFillCurve */

void osDrawPolyline (PolygonHandle arr, CanvasHandle canvas)
{
	Polyline (canvas->hDC, arr->data, arr->count);
}	/* osDrawPolygon */

void osDrawPolygon (PolygonHandle arr, CanvasHandle canvas)
{
	Polygon (canvas->hDC, arr->data, arr->count);
}	/* osDrawPolygon */

void osFillPolygon (PolygonHandle arr, CanvasHandle canvas)
{
	Polygon (canvas->hDC, arr->data, arr->count);
}	/* osFillPolygon */

/*	PA: two new routines that temporarily create and destroy a DISPLAY CanvasHandle.
		Use this HDC only for local use.
*/
CanvasHandle osGetTemporaryCanvas()
{
	CanvasHandle canvas = rmalloc(sizeof(*canvas));
	memset(canvas, 0, sizeof(*canvas));
	canvas->hDC = CreateDC ("DISPLAY",NULL,NULL,NULL);
	canvas->bInvalidated = FALSE;
	return canvas;
}	/* osCreateScreenHDC */

void osReleaseTemporaryCanvas(CanvasHandle canvas)
{
	DeleteDC (canvas->hDC);
	rfree(canvas);
}	/* osDestroyScreenHDC */


/*	osDrawBitmap must be used for drawing bitmaps on screen.
	For reasons of efficiency it uses memory device canvas, BitBlt, and bitmap handle.
*/
void osDrawBitmap (int destx, int desty,
					BitmapHandle bmp, CanvasHandle canvas
				   )
{
	HDC compatibleDC;
	HGDIOBJ prevObj;

	//	Create a compatible device canvas
	compatibleDC = CreateCompatibleDC (canvas->hDC);
	if (compatibleDC == NULL)
	{
		printf("CreateCompatibleDC failed.\n");
		exit(1);
	}

	//	Select bitmap into compatible device canvas
	prevObj = SelectObject (compatibleDC, bmp->hBitmap);
	SetMapMode(compatibleDC, GetMapMode (canvas->hDC));

	if (bmp->sourcesize.cx != bmp->destsize.cx || bmp->sourcesize.cy != bmp->destsize.cy)
	{
		if (!StretchBlt (canvas->hDC, destx, desty, bmp->destsize.cx, bmp->destsize.cy, compatibleDC, 0, 0, bmp->sourcesize.cx, bmp->sourcesize.cy, SRCCOPY))
		{
			printf("osDrawBitmap -> StretchBlt failed");
			exit(1);
		}
	}
	else
	{
		if (!BitBlt (canvas->hDC, destx, desty, bmp->sourcesize.cx, bmp->sourcesize.cy, compatibleDC, 0, 0, SRCCOPY))
		{
			printf("osDrawBitmap -> BitBlt failed");
			exit(1);
		}
	}

	SelectObject (compatibleDC, prevObj);
	DeleteDC (compatibleDC);
}	/* osDrawBitmap */

/*-----------------------------
	   Colors stuff
  -----------------------------*/

unsigned int osGetDialogColor()
{
	return GetSysColor(COLOR_3DFACE);
}

unsigned int osGetWindowColor()
{
	return GetSysColor(COLOR_WINDOW);
}

unsigned int osGetTextColor()
{
	return GetSysColor(COLOR_WINDOWTEXT);
}
