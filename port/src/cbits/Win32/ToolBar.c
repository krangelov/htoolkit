#include "ToolBar.h"
#include "DockBar.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include <windowsx.h>

#define CX_GRIPPER  3
#define CY_GRIPPER  3
#define CX_BORDER_GRIPPER 2
#define CY_BORDER_GRIPPER 2
#define CX_BORDER   1
#define CY_BORDER   1

#define HORZ  0x1000L
#define VERT  0x2000L
#define FLOAT 0x4000L

WNDPROC DefToolBarProc = NULL;

////////////////////////////////////////////
// Drawing & GDI utilities
////////////////////////////////////////////

static void FillSolidRect(HDC hDC, int x, int y, int cx, int cy, COLORREF clr)
{
	RECT rect;
	rect.left   = x;
	rect.top    = y;
	rect.right  = x + cx;
	rect.bottom = y + cy;

	SetBkColor(hDC, clr);
	ExtTextOut(hDC, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
}

static void Draw3dRect(HDC hDC, int x, int y, int cx, int cy, COLORREF clrTopLeft, COLORREF clrBottomRight)
{
	FillSolidRect(hDC, x, y, cx - 1, 1, clrTopLeft);
	FillSolidRect(hDC, x, y, 1, cy - 1, clrTopLeft);
	FillSolidRect(hDC, x + cx, y, -1, cy, clrBottomRight);
	FillSolidRect(hDC, x, y + cy, cx, -1, clrBottomRight);
}

static HBRUSH CreateHalftoneBrush()
{
	static WORD grayPattern[] = {0x5555, 0xAAAA, 0x5555, 0xAAAA, 0x5555, 0xAAAA, 0x5555, 0xAAAA};

	HBITMAP hGrayBitmap;
	HBRUSH hHalftoneBrush = NULL;

	hGrayBitmap = CreateBitmap(8, 8, 1, 1, &grayPattern);
	if (hGrayBitmap != NULL)
	{
		hHalftoneBrush = CreatePatternBrush(hGrayBitmap);
		DeleteObject(hGrayBitmap);
	}

	return hHalftoneBrush;
}

static void DrawDragRect(HDC hDC, RECT *pRect, SIZE size, RECT *pRectLast, SIZE sizeLast, HBRUSH hBrush, HBRUSH hBrushLast)
{
	RECT rect;
	HRGN hRgnNew, hRgnOutside, hRgnInside, hRgnLast, hRgnUpdate;
	HBRUSH hBrushOld;

	// first, determine the update region and select it
	rect = *pRect;
	InflateRect(&rect, -size.cx, -size.cy);
	IntersectRect(&rect, &rect, pRect);
	hRgnOutside = CreateRectRgnIndirect(pRect);
	hRgnInside  = CreateRectRgnIndirect(&rect);
	hRgnNew = CreateRectRgn(0, 0, 0, 0);
	CombineRgn(hRgnNew, hRgnOutside, hRgnInside, RGN_XOR);

	// find difference between new region and old region
	rect = *pRectLast;
	InflateRect(&rect, -sizeLast.cx, -sizeLast.cy);
	IntersectRect(&rect, &rect, pRectLast);
	SetRectRgn(hRgnOutside, pRectLast->left, pRectLast->top, pRectLast->right, pRectLast->bottom);
	SetRectRgn(hRgnInside,  rect.left,       rect.top,       rect.right,       rect.bottom);
	hRgnLast = CreateRectRgn(0, 0, 0, 0);
	CombineRgn(hRgnLast, hRgnOutside, hRgnInside, RGN_XOR);

	// only diff them if brushes are the same
	hRgnUpdate = NULL;
	if (hBrush == hBrushLast)
	{
		hRgnUpdate = CreateRectRgn(0, 0, 0, 0);
		CombineRgn(hRgnUpdate, hRgnLast, hRgnNew, RGN_XOR);
	}

	if (hBrush != hBrushLast && pRectLast != NULL)
	{
		// brushes are different -- erase old region first
		SelectClipRgn(hDC, hRgnLast);
		GetClipBox(hDC, &rect);
		hBrushOld = SelectObject(hDC, hBrushLast);
		PatBlt(hDC, rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top, PATINVERT);
		SelectObject(hDC, hBrushOld);
	}

	// draw into the update/new region
	SelectClipRgn(hDC, hRgnUpdate != NULL ? hRgnUpdate : hRgnNew);
	GetClipBox(hDC, &rect);
	hBrushOld = SelectObject(hDC, hBrush);
	PatBlt(hDC, rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top, PATINVERT);
	SelectObject(hDC, hBrushOld);
	SelectClipRgn(hDC, NULL);

	if (hRgnNew)     DeleteObject(hRgnNew);
	if (hRgnOutside) DeleteObject(hRgnOutside);
	if (hRgnInside)  DeleteObject(hRgnInside);
	if (hRgnLast)    DeleteObject(hRgnLast);
	if (hRgnUpdate)  DeleteObject(hRgnUpdate);
}

////////////////////////////////////////////
// utility functions
////////////////////////////////////////////

static BOOL ToolHitTest(HWND hToolBar, POINT point)
{
	RECT rect;
	int i, nButtons;
	TBBUTTON button;

	// now hit test against toolbar buttons
	nButtons = (int) SendMessage(hToolBar, TB_BUTTONCOUNT, 0, 0);
	for (i = 0; i < nButtons; i++)
	{
		if (SendMessage(hToolBar, TB_GETITEMRECT, i, (LPARAM)&rect))
		{
			if ((point.x >= rect.left && point.x <= rect.right &&
	        	 point.y >= rect.top  && point.y <= rect.bottom))
	        {
				if (SendMessage(hToolBar, TB_GETBUTTON, i, (LPARAM)&button) && (button.fsStyle & TBSTYLE_SEP) == 0)
					return TRUE;
			}
		}
	}

	return FALSE;
}

void GetToolBarSize(HWND hToolBar, SIZE *pSize)
{
	RECT rect;
	SIZE sz;
	int i, nButtons;
	LONG lStyle;

	lStyle = (LONG) GetWindowLongPtrW(hToolBar, GWLP_USERDATA);

	sz.cx = 0;
	sz.cy = 0;

	nButtons = (int) SendMessage(hToolBar, TB_BUTTONCOUNT, 0, 0);
	for (i = 0; i < nButtons; i++)
	{
		if (SendMessage(hToolBar, TB_GETITEMRECT, i, (LPARAM)&rect))
		{
			if (lStyle & HORZ)
			{
				sz.cx += rect.right-rect.left;
				sz.cy = max(sz.cy,rect.bottom-rect.top);
			}
			else
				if (lStyle & VERT)
				{
					sz.cx = max(sz.cx,rect.right-rect.left);
					sz.cy += rect.bottom-rect.top;
				}
		}
	}

	sz.cx += 4*CX_BORDER;
	sz.cy += 4*CY_BORDER;
	if (lStyle & HORZ)
		sz.cx += CX_BORDER_GRIPPER+CX_GRIPPER+CX_BORDER_GRIPPER;
	else
		if (lStyle & VERT)
			sz.cy += CY_BORDER_GRIPPER+CY_GRIPPER+CY_BORDER_GRIPPER;

	*pSize = sz;
}

////////////////////////////////////////////
// The DockContext
////////////////////////////////////////////

typedef struct
{
	HWND hToolBar;
	POINT ptLast;
	RECT rectLast;
	SIZE sizeLast;

	HWND hDesktopWnd;
	HDC  hDesktopDC;

	BOOL bDitherLast;
	HBRUSH hWhiteBrush, hDitherBrush;

	RECT rectDragHorz, rectFrameDragHorz;
	RECT rectDragVert, rectFrameDragVert;

	DWORD dwOverDockStyle;
	DWORD dwDockStyle;

	BOOL bFlip;
	BOOL bForceFrame;
} DockContext;

static BOOL InitDockContext(HWND hToolBar, POINT pos, DockContext *pCtxt)
{
	RECT rect;

	memset(pCtxt, 0, sizeof(DockContext));
	pCtxt->ptLast   = pos;
	pCtxt->hToolBar = hToolBar;
	pCtxt->bForceFrame = FALSE;
	pCtxt->bFlip = FALSE;
	pCtxt->bDitherLast = FALSE;
	pCtxt->dwDockStyle = ((LONG) GetWindowLongPtrW(hToolBar, GWLP_USERDATA)) & (HORZ | VERT);
	SetRectEmpty(&pCtxt->rectLast);

	// don't handle if capture already set
	if (GetCapture() != NULL)
		return FALSE;

	// get desktop window
	pCtxt->hDesktopWnd = GetDesktopWindow();
	if (!pCtxt->hDesktopWnd)
		return FALSE;

	// get the device context for the desktop window
	if (LockWindowUpdate(pCtxt->hDesktopWnd))
		pCtxt->hDesktopDC = GetDCEx(pCtxt->hDesktopWnd, NULL, DCX_WINDOW|DCX_CACHE|DCX_LOCKWINDOWUPDATE);
	else
		pCtxt->hDesktopDC = GetDCEx(pCtxt->hDesktopWnd, NULL, DCX_WINDOW|DCX_CACHE);

	if (!pCtxt->hDesktopDC)
		return FALSE;

	// get white brush
	pCtxt->hWhiteBrush = (HBRUSH) GetStockObject(WHITE_BRUSH);
	if (!pCtxt->hWhiteBrush)
	{
		ReleaseDC(pCtxt->hDesktopWnd, pCtxt->hDesktopDC);
		return FALSE;
	}

	// create dither brush
	pCtxt->hDitherBrush = CreateHalftoneBrush();
	if (!pCtxt->hDitherBrush)
	{
		DeleteObject(pCtxt->hWhiteBrush);
		ReleaseDC(pCtxt->hDesktopWnd, pCtxt->hDesktopDC);
		return FALSE;
	}

	GetWindowRect(hToolBar, &rect);
	MapWindowPoints(NULL, hToolBar, (LPPOINT) &rect, 2);

	// calculate inverted dragging rect
	if (pCtxt->dwDockStyle & HORZ)
	{
		pCtxt->rectDragHorz = rect;
		pCtxt->rectDragVert.left   = pos.x - (rect.bottom - rect.top)/2;
		pCtxt->rectDragVert.top    = rect.top;
		pCtxt->rectDragVert.right  = pCtxt->rectDragVert.left+(rect.bottom - rect.top );
		pCtxt->rectDragVert.bottom = pCtxt->rectDragVert.top +(rect.right  - rect.left);
	}
	else // vertical orientation
	{
		pCtxt->rectDragVert = rect;
		pCtxt->rectDragHorz.left   = rect.left;
		pCtxt->rectDragHorz.top    = pos.y - (rect.right - rect.left)/2;
		pCtxt->rectDragHorz.right  = pCtxt->rectDragHorz.left+(rect.bottom - rect.top );
		pCtxt->rectDragHorz.bottom = pCtxt->rectDragHorz.top +(rect.right  - rect.left);
	}

	// calculate frame dragging rectangle
	pCtxt->rectFrameDragHorz = pCtxt->rectDragHorz;
	pCtxt->rectFrameDragVert = pCtxt->rectDragVert;

	AdjustWindowRectEx(&pCtxt->rectFrameDragHorz, WS_THICKFRAME | WS_CAPTION, FALSE, WS_EX_PALETTEWINDOW);
	AdjustWindowRectEx(&pCtxt->rectFrameDragVert, WS_THICKFRAME | WS_CAPTION, FALSE, WS_EX_PALETTEWINDOW);
	InflateRect(&pCtxt->rectFrameDragHorz, -CX_BORDER*2, -CY_BORDER*2);
	InflateRect(&pCtxt->rectFrameDragVert, -CX_BORDER*2, -CY_BORDER*2);

	// set capture to the window which received this message
	SetCapture(pCtxt->hToolBar);

	return TRUE;
}

static void ReleaseDockContext(DockContext *pCtxt)
{
	DeleteObject(pCtxt->hDitherBrush);
	DeleteObject(pCtxt->hWhiteBrush);

	LockWindowUpdate(NULL);
	ReleaseDC(pCtxt->hDesktopWnd, pCtxt->hDesktopDC);

	ReleaseCapture();
}

////////////////////////////////////////////////////////

static void DrawDockRect(DockContext *pCtxt, BOOL bRemoveRect)
{
	SIZE size;
	RECT rect;
	HBRUSH hBrush = pCtxt->hWhiteBrush;

	// default to thin frame
	size.cx = CX_BORDER;
	size.cy = CY_BORDER;

	// determine new rect and size
	if (pCtxt->dwOverDockStyle & HORZ)
		rect = pCtxt->rectDragHorz;
	else
		if (pCtxt->dwOverDockStyle & VERT)
			rect = pCtxt->rectDragVert;
		else
		{
			// use thick frame instead
			size.cx = GetSystemMetrics(SM_CXFRAME) - CX_BORDER;
			size.cy = GetSystemMetrics(SM_CYFRAME) - CY_BORDER;

			if (((pCtxt->dwDockStyle & HORZ) && !pCtxt->bFlip) || ((pCtxt->dwDockStyle & VERT) && pCtxt->bFlip))
				rect = pCtxt->rectFrameDragHorz;
			else
				rect = pCtxt->rectFrameDragVert;
			hBrush = pCtxt->hDitherBrush;
		}

	if (bRemoveRect)
		size.cx = size.cy = 0;

	if (pCtxt->dwOverDockStyle & (VERT | HORZ))
	{
		// looks better one pixel in (makes the bar look pushed down)
		InflateRect(&rect, -CX_BORDER, -CY_BORDER);
	}

	// draw it and remember last size
	DrawDragRect(pCtxt->hDesktopDC, &rect, size, &pCtxt->rectLast, pCtxt->sizeLast,
			hBrush, pCtxt->bDitherLast ? pCtxt->hDitherBrush : pCtxt->hWhiteBrush);
	pCtxt->rectLast = rect;
	pCtxt->sizeLast = size;
	pCtxt->bDitherLast = (hBrush == pCtxt->hDitherBrush);
}

static BOOL CanDockBar(HWND hDockBar, RECT *pRect)
{
	RECT rect;

	GetWindowRect(hDockBar, &rect);
	if (rect.left == rect.right ) rect.right++;
	if (rect.top  == rect.bottom) rect.bottom++;

	return IntersectRect(&rect, &rect, pRect);
}

static DWORD CanDock(DockContext *pCtxt, HWND *phDockBar)
{
	RECT rect;
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame, GWLP_USERDATA);
	int nSize;

	if (((pCtxt->dwDockStyle & HORZ) && !pCtxt->bFlip) || ((pCtxt->dwDockStyle & VERT) && pCtxt->bFlip))
		rect = pCtxt->rectDragHorz;
	else
		rect = pCtxt->rectDragVert;
	nSize = min(rect.right-rect.left, rect.bottom-rect.top);
	rect.right  = rect.left + nSize;
	rect.bottom = rect.top  + nSize;

	if (CanDockBar(pData->hLeftBar, &rect))
	{
		if (phDockBar) *phDockBar = pData->hLeftBar;
		return VERT;
	}

	if (CanDockBar(pData->hTopBar, &rect))
	{
		if (phDockBar) *phDockBar = pData->hTopBar;
		return HORZ;
	}

	if (CanDockBar(pData->hRightBar, &rect))
	{
		if (phDockBar) *phDockBar = pData->hRightBar;
		return VERT;
	}

	if (CanDockBar(pData->hBottomBar, &rect))
	{
		if (phDockBar) *phDockBar = pData->hBottomBar;
		return HORZ;
	}

	return 0;
}

static void Move(DockContext *pCtxt, POINT pt)
{
	POINT ptOffset;

	ptOffset.x = pt.x - pCtxt->ptLast.x;
	ptOffset.y = pt.y - pCtxt->ptLast.y;

	// offset all drag rects to new position
	OffsetRect(&pCtxt->rectDragHorz,      ptOffset.x, ptOffset.y);
	OffsetRect(&pCtxt->rectFrameDragHorz, ptOffset.x, ptOffset.y);
	OffsetRect(&pCtxt->rectDragVert,      ptOffset.x, ptOffset.y);
	OffsetRect(&pCtxt->rectFrameDragVert, ptOffset.x, ptOffset.y);
	pCtxt->ptLast = pt;

	// if control key is down don't dock
	pCtxt->dwOverDockStyle = pCtxt->bForceFrame ? 0 : CanDock(pCtxt, NULL);

	// update feedback
	DrawDockRect(pCtxt, FALSE);
}

static void OnKey(DockContext *pCtxt, int nChar, BOOL bDown)
{
	switch (nChar)
	{
	case VK_CONTROL:
		if (pCtxt->bForceFrame == bDown)
			return;
		pCtxt->bForceFrame = bDown;
		break;
	case VK_SHIFT:
		if (pCtxt->bFlip == bDown)
			return;
		pCtxt->bFlip = bDown;
		break;
	default:
		return;
	}

	pCtxt->dwOverDockStyle = (pCtxt->bForceFrame) ? 0 : CanDock(pCtxt, NULL);
	DrawDockRect(pCtxt, FALSE);
}

static void EndDrag(DockContext *pCtxt)
{
	RECT rect;
	HWND hToolBar;
	HWND hDockBar = NULL;
	LONG lStyle;
	char *szCaption;
	int nLen;

	hToolBar = pCtxt->hToolBar;
	lStyle = GetWindowLongPtrW(hToolBar, GWLP_USERDATA);

	CanDock(pCtxt, &hDockBar);

	// undock the toolbar
	UndockToolBar(GetParent(hToolBar), hToolBar);
	if (hDockBar)
	{
		if (pCtxt->dwOverDockStyle & HORZ)
			rect = pCtxt->rectDragHorz;
		else
			rect = pCtxt->rectDragVert;

		SetWindowLongPtrW(hToolBar, GWLP_USERDATA, (lStyle & ~(VERT | HORZ | FLOAT)) | pCtxt->dwOverDockStyle);
		MapWindowPoints(NULL, hDockBar, (LPPOINT) &rect, 2);

		// dock it at the specified position
		DockToolBarToRect(hDockBar, hToolBar, rect);
	}
	else
	{
		if (((pCtxt->dwDockStyle & HORZ) && !pCtxt->bFlip) || ((pCtxt->dwDockStyle & VERT) && pCtxt->bFlip))
			rect = pCtxt->rectFrameDragHorz;
		else
			rect = pCtxt->rectFrameDragVert;

		SetWindowLongPtrW(hToolBar, GWLP_USERDATA, lStyle | FLOAT);

		szCaption = NULL;
		nLen = GetWindowTextLength(hToolBar);
		if (nLen > 0)
		{
			szCaption = malloc(nLen+1);
			if (!szCaption)
				szCaption = NULL;
			else
				GetWindowText(hToolBar,szCaption,nLen+1);
		}

		hDockBar = CreateWindowEx(WS_EX_TOOLWINDOW,
									"HDOCKBAR",
									szCaption,
									WS_CAPTION | WS_POPUP | WS_VISIBLE | WS_SYSMENU | WS_THICKFRAME,
									0,0,0,0,
									ghWndFrame,
									NULL,
									(HANDLE) ghModule,
									NULL);
		SetParent(hToolBar, hDockBar);
		MoveWindow(hDockBar, rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top, TRUE);

		free(szCaption);
	}
	RelayoutFrameBars();
}

static void StartDrag(HWND hToolBar, POINT pos)
{
	MSG  msg;
	DockContext ctxt;

	// handle pending WM_PAINT messages
	while (PeekMessage(&msg, NULL, WM_PAINT, WM_PAINT, PM_NOREMOVE))
	{
		if (!GetMessage(&msg, NULL, WM_PAINT, WM_PAINT))
			return;
		DispatchMessage(&msg);
	}

	if (!InitDockContext(hToolBar, pos, &ctxt))
		return;

	// get messages until capture lost or cancelled/accepted
	while (GetCapture() == hToolBar)
	{
		if (!GetMessage(&msg, NULL, 0, 0))
		{
			PostQuitMessage((int)msg.wParam);
			break;
		}

		switch (msg.message)
		{
		case WM_LBUTTONUP:
			EndDrag(&ctxt);
			goto end_loop;
		case WM_MOUSEMOVE:
			Move(&ctxt, msg.pt);
			break;
		case WM_KEYUP:
			OnKey(&ctxt, (int)msg.wParam, FALSE);
			break;
		case WM_KEYDOWN:
			OnKey(&ctxt, (int)msg.wParam, TRUE);
			if (msg.wParam == VK_ESCAPE)
				goto end_loop;
			break;
		case WM_RBUTTONDOWN:
			goto end_loop;

		// just dispatch rest of the messages
		default:
			DispatchMessage(&msg);
			break;
		}
	}

end_loop:
	DrawDockRect(&ctxt, TRUE);
	ReleaseDockContext(&ctxt);
}

LRESULT CALLBACK HToolBarFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	LRESULT lResult = 0;
	LONG lStyle = GetWindowLongPtrW(hWnd, GWLP_USERDATA);

	switch (uMsg)
	{
	case WM_NCCALCSIZE:
		if ((lStyle & FLOAT) == 0)
		{
			NCCALCSIZE_PARAMS *lpncsp = (NCCALCSIZE_PARAMS *) lParam;

			lpncsp->rgrc[0].left   += CX_BORDER*2;
			lpncsp->rgrc[0].top    += CY_BORDER*2;
			lpncsp->rgrc[0].right  -= CX_BORDER*2;
			lpncsp->rgrc[0].bottom -= CY_BORDER*2;

			if (lStyle & HORZ)
				lpncsp->rgrc[0].left += CX_BORDER_GRIPPER+CX_GRIPPER+CX_BORDER_GRIPPER;
			else
				if (lStyle & VERT)
					lpncsp->rgrc[0].top += CY_BORDER_GRIPPER+CY_GRIPPER+CY_BORDER_GRIPPER;

			return 0;
		};
		break;
	case WM_NCHITTEST:
		{
			POINT pos;

			pos.x = GET_X_LPARAM(lParam);
            pos.y = GET_Y_LPARAM(lParam);
            ScreenToClient(hWnd, &pos);
			if (ToolHitTest(hWnd, pos))
				return HTCLIENT;
			else
				return HTCAPTION;
		}
		break;
	case WM_NCPAINT:
		if ((lStyle & FLOAT) == 0)
		{
			HDC hDC;
			POINT pt;
			RECT rectClient, rectWindow, rect1, rect2;
			COLORREF clrBtnHilite, clrBtnShadow;

			clrBtnHilite = GetSysColor(COLOR_BTNHIGHLIGHT);
			clrBtnShadow = GetSysColor(COLOR_BTNSHADOW);

 			// get window DC that is clipped to the non-client area
			hDC = GetWindowDC(hWnd);

			GetClientRect(hWnd, &rectClient);
			GetWindowRect(hWnd, &rectWindow);

			pt.x = rectWindow.left;
			pt.y = rectWindow.top;
			ScreenToClient(hWnd, &pt);

			rectClient.left   -= pt.x;
			rectClient.top    -= pt.y;
			rectClient.right  -= pt.x;
			rectClient.bottom -= pt.y;

			ExcludeClipRect(hDC, rectClient.left, rectClient.top, rectClient.right, rectClient.bottom);

			rectWindow.right  = rectWindow.right-rectWindow.left;
			rectWindow.bottom = rectWindow.bottom-rectWindow.top;
			rectWindow.left   = 0;
			rectWindow.top    = 0;

			rect1 = rectWindow;
			rect2 = rectWindow;

			rect1.right  -= CX_BORDER;
			rect1.bottom -= CY_BORDER;
			rect2.top    += CY_BORDER*2;
			rect2.bottom -= CY_BORDER*2;

			// draw left and top dark line
			FillSolidRect(hDC, 0, rect2.top, CX_BORDER, rect2.bottom-rect2.top, clrBtnShadow);
			FillSolidRect(hDC, 0, 0, rectWindow.right, CY_BORDER, clrBtnShadow);

			// draw right and bottom dark line
			FillSolidRect(hDC, rect1.right, rect2.top, -CX_BORDER, rect2.bottom-rect2.top, clrBtnShadow);
			FillSolidRect(hDC, 0, rect1.bottom, rectWindow.right, -CY_BORDER, clrBtnShadow);

			// draw left and top hilite lines
			FillSolidRect(hDC, 1, rect2.top, CX_BORDER, rect2.bottom-rect2.top, clrBtnHilite);
			FillSolidRect(hDC, 0, 1, rectWindow.right, CY_BORDER, clrBtnHilite);

			// draw right and bottom
			FillSolidRect(hDC, rectWindow.right, rect2.top, -CX_BORDER, rect2.bottom-rect2.top, clrBtnHilite);
			FillSolidRect(hDC, 0, rectWindow.bottom, rectWindow.right, -CY_BORDER, clrBtnHilite);

			rectWindow.left += CX_BORDER*2;
			rectWindow.top += CY_BORDER*2;
			rectWindow.right -= CX_BORDER*2;
			rectWindow.bottom -= CY_BORDER*2;

			IntersectClipRect(hDC, rectWindow.left, rectWindow.top, rectWindow.right, rectWindow.bottom);

			// erase parts not drawn
			SendMessage(hWnd, WM_ERASEBKGND, (WPARAM)hDC, 0);

			// draw the gripper in the border
			if (lStyle & HORZ)
				Draw3dRect(hDC,rectWindow.left+CX_BORDER_GRIPPER,
									rectWindow.top,
									CX_GRIPPER, rectWindow.bottom-rectWindow.top,
									clrBtnHilite, clrBtnShadow);
			else
				if (lStyle & VERT)
					Draw3dRect(hDC,rectWindow.left,
									rectWindow.top+CY_BORDER_GRIPPER,
									rectWindow.right-rectWindow.left, CY_GRIPPER,
									clrBtnHilite, clrBtnShadow);

			// release context
			ReleaseDC(hWnd, hDC);
		}
		break;
	case WM_NCLBUTTONDOWN:
		if (wParam == HTCAPTION)
		{
			POINT pos;

			pos.x = GET_X_LPARAM(lParam);
			pos.y = GET_Y_LPARAM(lParam);
			ScreenToClient(hWnd, &pos);
			StartDrag(hWnd, pos);
		}
		break;
	case WM_DESTROY:
		{
			TBBUTTON tbb;
			while (SendMessage(hWnd, TB_GETBUTTON, 0, (LPARAM)&tbb))
			{
				ToolHandle toolItem = (ToolHandle) tbb.dwData;
				ToolHandle *prevToolRef;

				handleToolDestroy(toolItem);

				SendMessage(toolItem->hToolBar, TB_DELETEBUTTON, 0, 0);

				if (toolItem->action)
				{
					prevToolRef = &toolItem->action->toolProxies;
					while (*prevToolRef != toolItem)
						prevToolRef = &(*prevToolRef)->nextInAction;
					*prevToolRef = toolItem->nextInAction;
				}

				free(toolItem);
			}
			handleWindowDestroy(hWnd);
		}
		break;
	}

	lResult = CallWindowProc(DefToolBarProc, hWnd, uMsg, wParam, lParam);

	if (uMsg == WM_CREATE)
	{
		SendMessage (hWnd, TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);
		SendMessage (hWnd, TB_BUTTONSTRUCTSIZE, (WPARAM) sizeof(TBBUTTON), (LPARAM) 0);
	}

	return lResult;
};

WindowHandle osCreateToolBar(char *name, PositionType place, int band_num, int band_position, int offset)
{
	HWND hDockBar, hWnd;
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	LONG lStyle;

	switch (place)
	{
	case PosLeft:
		hDockBar = pFrameData->hLeftBar;
		lStyle = VERT;
		break;
	case PosTop:
		hDockBar = pFrameData->hTopBar;
		lStyle = HORZ;
		break;
	case PosRight:
		hDockBar = pFrameData->hRightBar;
		lStyle = VERT;
		break;
	case PosBottom:
		hDockBar = pFrameData->hBottomBar;
		lStyle = HORZ;
		break;
	default:
		return NULL;
	}

	hWnd = CreateWindow ("HTOOLBAR",
							name,
							  WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS
							| CCS_NOPARENTALIGN | CCS_NOMOVEY
							| CCS_NODIVIDER | CCS_NORESIZE | TBSTYLE_WRAPABLE
							| TBSTYLE_TOOLTIPS | TBSTYLE_AUTOSIZE | TBSTYLE_FLAT | TBSTYLE_LIST,
							0,0,0,0,
							hDockBar,
							(HMENU) NULL,
							(HANDLE) ghModule,
							NULL
							);
	SetWindowLongPtrW(hWnd, GWLP_USERDATA, lStyle);
	SendMessage(hWnd, TB_SETEXTENDEDSTYLE, 0, TBSTYLE_EX_MIXEDBUTTONS);
	SetWindowText(hWnd, name);
	DockToolBar(hDockBar, hWnd, band_num, band_position, offset);
	return hWnd;
}

void osDestroyToolBar(WindowHandle toolbar)
{
	UndockToolBar(GetParent(toolbar), toolbar);
	DestroyWindow(toolbar);
}

int osGetToolBarButtonCount(WindowHandle toolbar)
{
	return SendMessage(toolbar, TB_BUTTONCOUNT, 0, 0);
}

ToolHandle osInsertToolButton(ActionHandle action, WindowHandle toolBar, int pos)
{
	TBBUTTON   tbb;
	TBADDBITMAP tbab;
	ToolHandle btn;

	btn = malloc(sizeof(struct ToolHandle));
	if (!btn) return NULL;

	btn->hToolBar    = toolBar;
	btn->action      = action;

	// link the button to the chain
	btn->nextInAction = action->toolProxies;
	action->toolProxies = btn;

	if (action->bitmap)
	{
		tbab.hInst = NULL;
		tbab.nID   = (UINT_PTR) action->bitmap->hBitmap;
		tbb.iBitmap   = SendMessage(toolBar, TB_ADDBITMAP, 1, (LPARAM) &tbab);
	}
	else
	{
		tbb.iBitmap   = I_IMAGENONE;
	}

	tbb.idCommand = action->id;
	tbb.fsState   = action->enabled ? TBSTATE_ENABLED : 0;
	tbb.dwData    = (DWORD_PTR)btn;

	switch (btn->action->type)
	{
		case ACTION_NORMAL:
			tbb.fsStyle = TBSTYLE_BUTTON;
			break;
		case ACTION_CHECK:
			tbb.fsStyle = TBSTYLE_BUTTON | TBSTYLE_CHECK;
			break;
		case ACTION_RADIO:
			tbb.fsStyle = TBSTYLE_BUTTON;
			break;
		case ACTION_DROPDOWN:
			tbb.fsStyle = TBSTYLE_BUTTON | BTNS_WHOLEDROPDOWN;
			break;
	}

	tbb.iString = (INT_PTR) action->short_title;
	if (action->short_title)
		tbb.fsStyle |= BTNS_SHOWTEXT;

	if (pos > 0)
		SendMessage(toolBar,TB_INSERTBUTTON,pos,(LPARAM)&tbb);
	else
		SendMessage(toolBar,TB_ADDBUTTONS,  1,  (LPARAM)&tbb);

	RelayoutFrameBars();
	return btn;
}

ToolHandle osInsertToolLine(WindowHandle toolBar, int pos)
{
	TBBUTTON   tbb;
	ToolHandle btn;
	SIZE sz;

	btn = malloc(sizeof(struct ToolHandle));
	if (!btn) return NULL;

	btn->hToolBar    = toolBar;
	btn->action      = NULL;
	btn->nextInAction= NULL;

	tbb.iBitmap   = 1;
	tbb.idCommand = 0;
	tbb.fsState   = (BYTE)TBSTATE_ENABLED;
	tbb.fsStyle   = (BYTE)TBSTYLE_SEP;
	tbb.dwData    = (DWORD_PTR)btn;
	tbb.iString   = 0;

	if (pos > 0)
		SendMessage(toolBar,TB_INSERTBUTTON,pos,(LPARAM)&tbb);
	else
		SendMessage(toolBar, TB_ADDBUTTONS, 1,  (LPARAM)&tbb);

	SendMessage(toolBar, TB_GETMAXSIZE, 0, (LPARAM) &sz);
	sz.cx += CX_BORDER*4;
	sz.cy += CY_BORDER*4;
	SetWindowPos(toolBar, NULL, 0, 0, sz.cx, sz.cy, SWP_NOMOVE | SWP_NOZORDER);
	RelayoutFrameBars();

	return btn;
}

int osGetToolItemPos(ToolHandle toolItem)
{
	int i, nButtons;
	TBBUTTON tbb;

	nButtons = (int) SendMessage(toolItem->hToolBar, TB_BUTTONCOUNT, 0, 0);

	for (i = 0; i < nButtons; i++)
	{
		if (SendMessage(toolItem->hToolBar, TB_GETBUTTON, i, (LPARAM)&tbb))
		{
			if (((ToolHandle) tbb.dwData) == toolItem)
				return i;
		}
	}

	return -1;
}

void osDestroyToolItem(ToolHandle toolItem)
{
	ToolHandle *prevToolRef;

	handleToolDestroy(toolItem);

	SendMessage(toolItem->hToolBar, TB_DELETEBUTTON, osGetToolItemPos(toolItem), 0);

	if (toolItem->action)
	{
		prevToolRef = &toolItem->action->toolProxies;
		while (*prevToolRef != toolItem)
			prevToolRef = &(*prevToolRef)->nextInAction;
		*prevToolRef = toolItem->nextInAction;
	}

	free(toolItem);

	RelayoutFrameBars();
}
