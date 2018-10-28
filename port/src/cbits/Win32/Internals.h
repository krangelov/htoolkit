#ifndef ITERNALS_H
#define ITERNALS_H

#include "ActionsMap.h"

extern BOOL gInKey;
extern int gCurChar;
extern HMODULE ghModule;
extern HWND ghWndFrame;

extern unsigned int GetModifiers();

extern WindowHandle checkWindow(HWND hWnd, char *className);

extern void SetupLogBrush(LOGBRUSH *plb, BOOL bSetSolid, int hatchStyle, BitmapHandle patBmp);

typedef struct StatusContext
{
	struct StatusContext *next;
	char tip[0];
} *StatusContext;

typedef struct
{
	HWND hClientWnd;
	int DocumentInterface;
	LPWSTR lpszAppName;
	LPWSTR lpszAppVersion;
	ActionsMap *pActionsMap;

	HWND hLeftBar, hTopBar, hRightBar, hBottomBar;
	HWND hStatusBar;
	StatusContext statusContexts;
	IndicatorHandle first_indicator, last_indicator;
} FrameData;

typedef struct
{
	POINT Origin;
	SIZE DomainSize;
	SIZE LineSize, PageSize;
	BOOL bInDragMode;

	COLORREF foreColor;
	COLORREF backColor;
	HBRUSH hBackBrush;
	int hatchStyle;
	BitmapHandle patBmp;

	BOOL enabled;
	int disabledCtrlsCount;
	HWND *disabledCtrls;

	HWND hTooltip;

	SIZE MinTrackSize;

	int windowPos;
	RECT windowPosRect;
} WindowData;

extern void rfree(void *ptr);
extern void *rmalloc(DWORD bytes);

void osForceContainerReLayout(HWND hCtrl);

void RefreshStatusBarIndicators();

void DrawCheckListBoxItem(LPDRAWITEMSTRUCT lpDIS);

void SaveWindowState(HWND hWnd);
void RestoreWindowState(HWND hWnd, int nShow);

void osActivateAction(int id);

void osDestroyToolItem(ToolHandle toolItem);
void osDestroyMenu(MenuHandle handle);

void getWindowClipRgn(HWND hWnd, HRGN hRgn);

#ifdef WINE_TARGET
size_t
pslen(PortString s);

PortString
psdup(PortString s);

int
pscmp(PortString s1, PortString s2);

void
pscpy(PortString s1, PortString s2);

void
pscat(PortString s1, PortString s2);

void
ftops(double f, WCHAR *szOutput);
#else
#define pslen wcslen
#define psdup wcsdup
#define pscmp wcscmp
#define pscpy wcscpy
#define pscat wcscat
#define ftops(f,szOutput) wsprintfW(szOutput, L"%f", f);
#endif

#endif
