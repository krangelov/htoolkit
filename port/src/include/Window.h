#ifndef WINDOW_H
#define WINDOW_H

#include "Types.h"

#define shiftBIT 1
#define ctrlBIT  2
#define altBIT   4

#define evMouseMove   	    1
#define evMouseLeftDown     4
#define evMouseRightDown    5
#define evMouseDoubleClick  6
#define evMouseDrag    	    7
#define evMouseLeftUp       8
#define evMouseRightUp      9

#define evKeyDown   		10
#define evKeyStillDown  	11
#define evKeyUp    			12
#define evKeyLost			13

#define kbBackSpace 		8
#define kbTab				9
#define kbEnter				13
#define kbEscape			27
#define kbBegin 			1003
#define kbClear				1004
#define kbDelete			1005
#define kbDown				1006
#define kbEnd				1007
#define kbF1				1010
#define kbF2				1011
#define kbF3				1012
#define kbF4				1013
#define kbF5				1014
#define kbF6				1015
#define kbF7				1016
#define kbF8				1017
#define kbF9				1018
#define kbF10				1019
#define kbF11				1020
#define kbF12				1021
#define kbF13				1022
#define kbF14				1023
#define kbF15				1024
#define kbHelp				1025
#define kbLeft				1026
#define kbPgDown			1027
#define kbPgUp				1028
#define kbRight				1029
#define kbUp				1030



void osMoveResizeControl(WindowHandle ctrl, int x, int y, int w, int h);
void osGetControlRect(WindowHandle ctrl, int *res);
void osSetControlEnabled(WindowHandle ctrl, BOOL enabled);
BOOL osGetControlEnabled(WindowHandle ctrl);
void osSetControlVisible(WindowHandle ctrl, BOOL visible);
BOOL osGetControlVisible(WindowHandle ctrl);
void osSetControlTip(WindowHandle ctrl, char *text);
char *osGetControlTip(WindowHandle ctrl);

void osInvalidateWindow(WindowHandle window);
void osInvalidateWindowRect(WindowHandle window, int left, int top, int right, int bottom);

WindowHandle osCreateWindow();
WindowHandle osCreateDialog(WindowHandle parent);
WindowHandle osCreateCompoundControl(WindowHandle form);
void osGetCompoundControlReqSize(WindowHandle compound, int *res);
void osSetWindowColor(WindowHandle window, int foreColor, int backColor, int hatchStyle, BitmapHandle patBmp);
char *osGetWindowTitle(WindowHandle window);
void osSetWindowTitle(WindowHandle window, char *txt);
void osGetWindowViewSize(WindowHandle window, int *res);
void osSetWindowViewSize(WindowHandle window, int w, int h);
void osSetWindowVisible(WindowHandle window, BOOL visible);
BOOL osGetWindowVisible(WindowHandle window);
void osShowWindow(WindowHandle window);
void osRunDialog(WindowHandle window);
void osSetWindowDomainSize(WindowHandle window, int cx, int cy);
void osSetWindowScrollOrigin(WindowHandle window, int x, int y);
void osGetWindowScrollOrigin(WindowHandle window, int *res);
void osGetWindowPageSize(WindowHandle window, int *res);
void osSetWindowPageSize(WindowHandle window, int cx, int cy);
void osGetWindowLineSize(WindowHandle window, int *res);
void osSetWindowLineSize(WindowHandle window, int cx, int cy);
BOOL osDismissWindow(WindowHandle window);
void osDestroyWindow(WindowHandle window);
void osSetWindowEnabled(WindowHandle ctrl, BOOL enabled);
BOOL osGetWindowEnabled(WindowHandle ctrl);
CanvasHandle osGetWindowCanvas(WindowHandle window);
void osReleaseWindowCanvas(WindowHandle window,CanvasHandle canvas);

void osSetWindowPosition(WindowHandle window, int position, int x0, int y0, int x1, int y1);
void osGetWindowRect(WindowHandle window, int *res);
void osSetWindowResizeable(WindowHandle window, int resizeable);

void osReLayoutContainer(WindowHandle window);

void osSetDialogMinSize(WindowHandle dialog, int w, int h);

#endif
