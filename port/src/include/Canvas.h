#ifndef CANVAS_H
#define CANVAS_H

#include "Types.h"

extern int osMMtoVPixels(double);
extern int osMMtoHPixels(double);

extern void osInitCanvas (int,int,unsigned int,unsigned int,int,int,int,int,unsigned char *,BOOL,int,void*,FontHandle,CanvasHandle,BOOL);
extern void osDoneCanvas (CanvasHandle);

extern void osChangeCanvasPen(int,int,unsigned int,unsigned int,int,int,int,int,unsigned char *,BOOL,int,void*,FontHandle,CanvasHandle);

/*	Operations to create, modify, and destroy polygon shapes.
*/
extern PolygonHandle osCreatePolygon(int);
extern void osAddPolygonPoint(PolygonHandle,int,int);
extern void osDeletePolygon(PolygonHandle);

extern void osRotateCanvas(double angle, CanvasHandle canvas);
extern void osScaleCanvas(double dScaleX, double dScaleY, CanvasHandle canvas);
extern void osShearCanvas(double dShearX, double dShearY, CanvasHandle canvas);
extern void osTranslateCanvas(double dDeltaX, double dDeltaY, CanvasHandle canvas);

extern void osDrawPoint (int,int,CanvasHandle canvas);

extern void osDrawLine (int,int,int,int,CanvasHandle canvas);

extern void osDrawChar (int,int,char,CanvasHandle canvas);
extern void osDrawString (int,int,char*,CanvasHandle canvas);

extern void osDrawRect (int,int,int,int,CanvasHandle canvas);
extern void osFillRect (int,int,int,int,CanvasHandle canvas);

extern void osDrawOval (int,int,int,int,CanvasHandle canvas);
extern void osFillOval (int,int,int,int,CanvasHandle canvas);

extern void osDrawCurve (int x0, int y0, int x1, int y1, float from, float to, BOOL clockwise, CanvasHandle canvas);
extern void osFillCurve (int x0, int y0, int x1, int y1, float from, float to, BOOL clockwise, CanvasHandle canvas);

extern void osDrawPolyline (PolygonHandle,CanvasHandle canvas);

extern void osDrawPolygon (PolygonHandle,CanvasHandle canvas);
extern void osFillPolygon (PolygonHandle,CanvasHandle canvas);

extern void osDrawBitmap (int,int,BitmapHandle,CanvasHandle canvas);

//	Routines that temporarily create and destroy a DISPLAY OSPictContext. Use this OSPictContext only locally.
extern CanvasHandle osGetTemporaryCanvas();
extern void osReleaseTemporaryCanvas(CanvasHandle canvas);

unsigned int osGetDialogColor();
unsigned int osGetWindowColor();
unsigned int osGetTextColor();

#endif
