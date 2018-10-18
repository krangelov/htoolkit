#include "Window.h"
#include "Action.h"
#include "DockBar.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include <windowsx.h>

unsigned int GetModifiers()
{
    return (( GetAsyncKeyState (VK_SHIFT)   ? shiftBIT : 0) |
            ( GetAsyncKeyState (VK_CONTROL) ? ctrlBIT  : 0) |
            ( GetAsyncKeyState (VK_MENU)    ? altBIT   : 0));
}

static void moveControls(HWND hWnd, int dx, int dy)
{
    RECT rect;
    HWND hCtrl = GetTopWindow(hWnd);

    while (hCtrl)
    {
		if (IsWindowVisible(hCtrl))
		{
			GetWindowRect(hCtrl,&rect);
			MapWindowPoints(NULL, hWnd, (LPPOINT) &rect, 2);
			SetWindowPos(hCtrl,NULL,rect.left+dx,rect.top+dy,0,0,SWP_NOSIZE|SWP_NOZORDER);
		}

        hCtrl = GetNextWindow(hCtrl,GW_HWNDNEXT);
    }
}

void getWindowClipRgn(HWND hWnd, HRGN hRgn)
{
    RECT rect;
    HRGN hTmpRgn;
    HWND hCtrl = GetTopWindow(hWnd);

    while (hCtrl)
    {
		if (IsWindowVisible(hCtrl))
		{
			GetWindowRect(hCtrl,&rect);
			MapWindowPoints(NULL, hWnd, (LPPOINT) &rect, 2);
			hTmpRgn = CreateRectRgn(rect.left, rect.top, rect.right, rect.bottom);
			CombineRgn(hRgn, hRgn, hTmpRgn, RGN_DIFF);
			DeleteObject(hTmpRgn);
		}

        hCtrl = GetNextWindow(hCtrl,GW_HWNDNEXT);
    }
}

BOOL gInKey   = FALSE;
BOOL gCurChar = 0;

int CheckVirtualKeyCode (int keycode)
{
    switch (keycode)
    {
        case VK_UP:     return kbUp;
        case VK_DOWN:   return kbDown;
        case VK_LEFT:   return kbLeft;
        case VK_RIGHT:  return kbRight;
        case VK_PRIOR:  return kbPgUp;
        case VK_NEXT:   return kbPgDown;
        case VK_END:    return kbEnd;
        case VK_HOME:   return kbBegin;
        case VK_BACK:   return kbBackSpace;
        case VK_DELETE: return kbDelete;
        case VK_RETURN: return kbEnter;
        case VK_ESCAPE: return kbEscape;
        case VK_TAB:    return kbTab;
        case VK_HELP:   return kbHelp;
        case VK_F1:     return kbF1;
        case VK_F2:     return kbF2;
        case VK_F3:     return kbF3;
        case VK_F4:     return kbF4;
        case VK_F5:     return kbF5;
        case VK_F6:     return kbF6;
        case VK_F7:     return kbF7;
        case VK_F8:     return kbF8;
        case VK_F9:     return kbF9;
        case VK_F10:    return kbF10;
        case VK_F11:    return kbF11;
        case VK_F12:    return kbF12;
        case VK_CLEAR:  return kbClear;
    }

    return 0;
}

static void DrawSizeGripLine(HDC hDC, int right, int bottom, COLORREF rgbColor, int nIndex)
{
	HPEN hPen, hOldPen;

	hPen = CreatePen(PS_SOLID, 1, rgbColor);
	hOldPen = SelectObject(hDC, hPen);
	{
		MoveToEx(hDC,right-5+nIndex,bottom, NULL);
		LineTo(hDC,right,bottom-5+nIndex);

		MoveToEx(hDC,right-9+nIndex,bottom, NULL);
		LineTo(hDC,right,bottom-9+nIndex);

		MoveToEx(hDC,right-13+nIndex,bottom, NULL);
		LineTo(hDC,right,bottom-13+nIndex);
	}
	SelectObject(hDC, hOldPen);
	DeleteObject(hPen);
}

static void DrawSizeGrip(HWND hWnd, HDC hDC)
{
	RECT rect;
	GetClientRect(hWnd, &rect);

	DrawSizeGripLine(hDC,rect.right,rect.bottom,RGB(255,255,255),0);
	DrawSizeGripLine(hDC,rect.right,rect.bottom,GetSysColor(COLOR_3DSHADOW),1);
	DrawSizeGripLine(hDC,rect.right,rect.bottom,GetSysColor(COLOR_3DSHADOW),2);
	DrawSizeGripLine(hDC,rect.right,rect.bottom,GetSysColor(COLOR_3DFACE),3);
}

static void CenterToScreen(POINT *pos, SIZE sz)
{
	HDC screen;
	screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
	if (screen==NULL)
	{
		printf("CreateDC returned NULL.\n");
		exit(1);
	}
	pos->x = (GetDeviceCaps(screen,HORZRES)-sz.cx)/2;
	pos->y = (GetDeviceCaps(screen,VERTRES)-sz.cy)/2;
	DeleteDC (screen);
}

static void SetupWindowPosition(HWND hWnd)
{
	SIZE sz;
	POINT pos;
	HWND hParent;
	WindowData *pData;
	RECT rc;

	pData = (WindowData *) GetWindowLongPtr(hWnd,GWLP_USERDATA);

	pos.x = pData->windowPosRect.left;
	pos.y = pData->windowPosRect.top;
	sz.cx = pData->windowPosRect.right-pData->windowPosRect.left;
	sz.cy = pData->windowPosRect.bottom-pData->windowPosRect.top;

	hParent = GetParent(hWnd);

	switch (pData->windowPos)
	{
	case 0:
		break;
	case 1:
		CenterToScreen(&pos, sz);
		break;
	case 2:
		if (hParent)
			GetClientRect(hParent, &rc);
		else
			GetWindowRect(GetWindow(hWnd, GW_OWNER), &rc);

		pos.x = ((rc.right+rc.left)-sz.cx)/2;
		pos.y = ((rc.bottom+rc.top)-sz.cy)/2;
		break;
	case 3:
		GetCursorPos(&pos);
		if (hParent)
			MapWindowPoints(NULL, hParent, &pos, 1);
		break;
	default:
		return;
	}

	MoveWindow(hWnd,pos.x,pos.y,sz.cx,sz.cy,TRUE);
}

LRESULT CALLBACK HWindowSharedFunction(WNDPROC pDefWindowProc, HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    WindowData *pData = (WindowData *) GetWindowLongPtr(hWnd,GWLP_USERDATA);

    switch (uMsg)
    {
    case WM_CREATE:
        {
            pData = (WindowData *) rmalloc(sizeof(WindowData));
            pData->Origin.x = 0;
            pData->Origin.y = 0;
            pData->DomainSize.cx = 0;
            pData->DomainSize.cy = 0;
            pData->LineSize.cx = 1;
            pData->LineSize.cy = 1;
            pData->PageSize.cx = 10;
            pData->PageSize.cy = 10;
            pData->bInDragMode = FALSE;
            pData->foreColor = RGB(0,0,0);
            pData->backColor = RGB(255,255,255);
            pData->hatchStyle = 0;
			pData->patBmp = NULL;
			pData->hBackBrush = (HBRUSH) GetClassLongPtr(hWnd, GCLP_HBRBACKGROUND);
            pData->enabled = TRUE;
            pData->disabledCtrlsCount = 0;
			pData->disabledCtrls = NULL;
            pData->hTooltip = NULL;
			pData->MinTrackSize.cx = 0;
            pData->MinTrackSize.cy = 0;
            pData->windowPos = -1;
			SetWindowLongPtr(hWnd,GWLP_USERDATA,(LONG_PTR) pData);

			SetFocus(hWnd);
        }
        break;
    case WM_CLOSE:
        handleWindowDismiss(hWnd);
        return 0;
    case WM_DESTROY:
    	handleWindowDestroy(hWnd);
        if (pData->hBackBrush)
        {
            DeleteObject(pData->hBackBrush);
            pData->hBackBrush = NULL;
        }

        free(pData);
        break;
    case WM_COMMAND:
        if (lParam != 0)
        {
            char buffer[20];
            HWND hCtrl = (HWND) lParam;

            GetClassName(hCtrl,buffer,sizeof(buffer));
            if (_stricmp(buffer, "ComboBox") == 0)
            {
                if (HIWORD(wParam) == CBN_SELENDOK)
                    handleControlCommand(hCtrl);
            }
            else
                if (_stricmp(buffer, "ListBox") == 0)
                {
                    if (HIWORD(wParam) == LBN_SELCHANGE)
                        handleControlCommand(hCtrl);
                }
                else
                    if (_stricmp(buffer, "Button") == 0)
                    {
                    	if ((GetWindowLong(hCtrl, GWL_STYLE) & BS_AUTORADIOBUTTON) == BS_AUTORADIOBUTTON)
                    	{
							HWND hNextCtrl = hCtrl;
							for (;;)
							{
								hNextCtrl = (WindowHandle) GetWindowLongPtr(hNextCtrl, GWLP_USERDATA);
								if (hNextCtrl == hCtrl) break;

								SendMessage(hNextCtrl,BM_SETCHECK,BST_UNCHECKED,0);
							}
                    	}

						handleControlCommand(hCtrl);
					}
        }
        else
			osActivateAction(LOWORD(wParam));
        break;
    case WM_NOTIFY:
    	{
    		NMHDR *pNMHDR = (NMHDR *) lParam;

			if (pNMHDR->code == UDN_DELTAPOS)
			{
				if (((LPNM_UPDOWN)pNMHDR)->iDelta < 0)
					handleTrackBarIncrement(pNMHDR->hwndFrom);
				else
					handleTrackBarDecrement(pNMHDR->hwndFrom);
			}
			else
			{
				if (pNMHDR->hwndFrom != hWnd)
					SendMessage(pNMHDR->hwndFrom, uMsg, wParam, lParam);
			}
    	}
    	break;
    case WM_PAINT:
        {
            HRGN theRegion;
            RECT updRect;
            PAINTSTRUCT ps;
            CanvasHandle canvas;

            BeginPaint(hWnd, &ps);

            SetViewportExtEx(ps.hdc,pData->DomainSize.cx,pData->DomainSize.cy,NULL);
            SetWindowExtEx(ps.hdc,pData->DomainSize.cx,pData->DomainSize.cy,NULL);
            SetViewportOrgEx(ps.hdc,-pData->Origin.x,-pData->Origin.y,NULL);

            updRect.left   = ps.rcPaint.left   + pData->Origin.x;
            updRect.top    = ps.rcPaint.top    + pData->Origin.y;
            updRect.right  = ps.rcPaint.right  + pData->Origin.x;
            updRect.bottom = ps.rcPaint.bottom + pData->Origin.y;

            theRegion = CreateRectRgn(ps.rcPaint.left, ps.rcPaint.top, ps.rcPaint.right, ps.rcPaint.bottom);
            getWindowClipRgn(hWnd, theRegion);
            SelectClipRgn (ps.hdc, theRegion);
            DeleteObject(theRegion);

            canvas = rmalloc(sizeof(*canvas));
            memset(canvas, 0, sizeof(*canvas));
            canvas->hDC = ps.hdc;
            canvas->bInvalidated = TRUE;
            handleWindowPaint(hWnd,canvas, updRect.left, updRect.top, updRect.right, updRect.bottom);
            rfree(canvas);

			if (GetParent(hWnd) == NULL && (GetWindowLong(hWnd, GWL_STYLE) & WS_THICKFRAME))
				DrawSizeGrip(hWnd, ps.hdc);

            EndPaint(hWnd, &ps);
        }
        break;
    case WM_ERASEBKGND:
        return TRUE;
	case WM_SIZING:
		if (GetParent(hWnd) == NULL && (GetWindowLong(hWnd, GWL_STYLE) & WS_THICKFRAME))
		{
			RECT rect;
			GetClientRect(hWnd,&rect);

			rect.left = rect.right-14;
			rect.top  = rect.bottom-14;
			InvalidateRect(hWnd,&rect,TRUE);
		}
		break;
    case WM_SIZE:
        {
            RECT rect;
            SCROLLINFO si;

            int nWidth  = LOWORD(lParam);
            int nHeight = HIWORD(lParam);

            int  nLimitX = pData->DomainSize.cx - nWidth;
            int  nLimitY = pData->DomainSize.cy - nHeight;

            si.cbSize = sizeof(si);
            si.fMask  = SIF_PAGE;

            si.nPage = nWidth;
            SetScrollInfo(hWnd, SB_HORZ, &si, TRUE);

            si.nPage = nHeight;
            SetScrollInfo(hWnd, SB_VERT, &si, TRUE);

            if (nLimitX > 0 && pData->Origin.x > nLimitX) pData->Origin.x = nLimitX;
            if (nLimitY > 0 && pData->Origin.y > nLimitY) pData->Origin.y = nLimitY;

            GetClientRect(hWnd,&rect);
            handleWindowResize(hWnd,rect.right-rect.left,rect.bottom-rect.top);
            handleContainerReLayout(hWnd);
        }
        break;
    case WM_VSCROLL:
    	if (lParam == 0)
        {
            RECT rect;
            int nOldPos = pData->Origin.y;
            int nPos = pData->Origin.y;
            int nLimit;

            GetClientRect(hWnd,&rect);
            nLimit = pData->DomainSize.cy - (rect.bottom-rect.top);

            switch (LOWORD(wParam))
            {
            case SB_BOTTOM:
                nPos = nLimit;
                break;
            case SB_LINEDOWN:
                nPos = min(nLimit,pData->Origin.y + pData->LineSize.cy);
                break;
            case SB_LINEUP:
                nPos = max(0,pData->Origin.y - pData->LineSize.cy);
                break;
            case SB_PAGEDOWN:
                nPos = min(nLimit,pData->Origin.y + pData->PageSize.cy);
                break;
            case SB_PAGEUP:
                nPos = max(0,pData->Origin.y - pData->PageSize.cy);
                break;
            case SB_THUMBPOSITION:
            case SB_THUMBTRACK:
                nPos = (HIWORD(wParam)/pData->LineSize.cy)*pData->LineSize.cy;
                break;
            case SB_TOP:
                nPos = 0;
                break;
            }

            if (nPos != nOldPos)
            {
                SetScrollPos(hWnd,SB_VERT,nPos,TRUE);
                InvalidateRect(hWnd,NULL,TRUE);
                moveControls(hWnd,0,pData->Origin.y-nPos);

                pData->Origin.y = nPos;

                handleWindowScroll(hWnd,pData->Origin.x,pData->Origin.y);
            }
        }
        else
        {
			handleControlCommand((WindowHandle) lParam);
		}
        break;
    case WM_HSCROLL:
    	if (lParam == 0)
        {
            RECT rect;
            int nOldPos = pData->Origin.x;
            int nPos = pData->Origin.x;
            int nLimit;

            GetClientRect(hWnd,&rect);
            nLimit = pData->DomainSize.cx - (rect.right-rect.left);

            switch (LOWORD(wParam))
            {
            case SB_BOTTOM:
                nPos = nLimit;
                break;
            case SB_LINEDOWN:
                nPos = min(nLimit,pData->Origin.x + pData->LineSize.cx);
                break;
            case SB_LINEUP:
                nPos = max(0,pData->Origin.x - pData->LineSize.cx);
                break;
            case SB_PAGEDOWN:
                nPos = min(nLimit,pData->Origin.x + pData->PageSize.cx);
                break;
            case SB_PAGEUP:
                nPos = max(0,pData->Origin.x - pData->PageSize.cx);
                break;
            case SB_THUMBPOSITION:
            case SB_THUMBTRACK:
                nPos = (HIWORD(wParam)/pData->LineSize.cx)*pData->LineSize.cx;
                break;
            case SB_TOP:
                nPos = 0;
                break;
            }

            if (nPos != nOldPos)
            {
                SetScrollPos(hWnd,SB_HORZ,nPos,TRUE);
                moveControls(hWnd,pData->Origin.x-nPos,0);
                InvalidateRect(hWnd,NULL,TRUE);

                pData->Origin.x = nPos;

                handleWindowScroll(hWnd,pData->Origin.x,pData->Origin.y);
            }
        }
        else
        {
			handleControlCommand((WindowHandle) lParam);
		}
        break;
    case WM_MOUSEWHEEL:
        {
            RECT rect;
            int nLimit;
            int nDelta = GET_WHEEL_DELTA_WPARAM(wParam)/WHEEL_DELTA;
            int nPos, nOldPos = pData->Origin.y;

            GetClientRect(hWnd,&rect);
            nLimit = pData->DomainSize.cy - (rect.bottom-rect.top);

            nPos = nOldPos - nDelta*pData->LineSize.cy;
            nPos = max(0,min(nLimit,nPos));

            if (nPos != nOldPos)
            {
                SetScrollPos(hWnd,SB_VERT,nPos,TRUE);
                moveControls(hWnd,0,pData->Origin.y-nPos);
                InvalidateRect(hWnd,NULL,TRUE);

                pData->Origin.y = nPos;

                handleWindowScroll(hWnd,pData->Origin.x,pData->Origin.y);
            }
        }
        break;
    case WM_CAPTURECHANGED:
    	if (pData->enabled)
        {
            int cx = GET_X_LPARAM(lParam);
            int cy = GET_Y_LPARAM(lParam);

            if (pData->bInDragMode)
            {
                pData->bInDragMode = FALSE;
                handleWindowMouse(hWnd,evMouseLeftUp,pData->Origin.x+cx,pData->Origin.y+cy,GetModifiers());
            }
        }
        break;
    case WM_MOUSEMOVE:
    	if (pData->enabled)
        {
            int cx = GET_X_LPARAM(lParam);
            int cy = GET_Y_LPARAM(lParam);

            if (pData->bInDragMode)
                handleWindowMouse(hWnd,evMouseDrag,pData->Origin.x+cx,pData->Origin.y+cy,GetModifiers());
            else
                handleWindowMouse(hWnd,evMouseMove,pData->Origin.x+cx,pData->Origin.y+cy,GetModifiers());
        }
        break;
    case WM_LBUTTONDOWN:
    	if (pData->enabled)
        {
            POINT pos;

            pos.x = pData->Origin.x+GET_X_LPARAM(lParam);
            pos.y = pData->Origin.y+GET_Y_LPARAM(lParam);

			pData->bInDragMode = TRUE;
			handleWindowMouse(hWnd,evMouseLeftDown,pos.x,pos.y,GetModifiers());

			SetCapture(hWnd);
        }
        break;
    case WM_LBUTTONDBLCLK:
    	if (pData->enabled)
        {
            POINT pos;

            pos.x = pData->Origin.x+GET_X_LPARAM(lParam);
            pos.y = pData->Origin.y+GET_Y_LPARAM(lParam);

            handleWindowMouse(hWnd,evMouseDoubleClick,pos.x,pos.y,GetModifiers());
        }
        break;
    case WM_LBUTTONUP:
    	if (pData->enabled)
        {
            POINT pos;
            int cx, cy;

            cx = GET_X_LPARAM(lParam);
            cy = GET_Y_LPARAM(lParam);
            pos.x = pData->Origin.x+cx;
            pos.y = pData->Origin.y+cy;

            pData->bInDragMode = FALSE;

            handleWindowMouse(hWnd,evMouseLeftUp,pData->Origin.x+cx,pData->Origin.y+cy,GetModifiers());

            ReleaseCapture();
        }
        break;
    case WM_RBUTTONDOWN:
    	if (pData->enabled)
        {
            POINT pos;

            pos.x = pData->Origin.x+GET_X_LPARAM(lParam);
            pos.y = pData->Origin.y+GET_Y_LPARAM(lParam);

            handleWindowMouse(hWnd,evMouseRightDown,pos.x,pos.y,GetModifiers());
        }
        break;
    case WM_RBUTTONUP:
    	if (pData->enabled)
        {
            POINT pos;

            pos.x = pData->Origin.x+GET_X_LPARAM(lParam);
            pos.y = pData->Origin.y+GET_Y_LPARAM(lParam);

			handleWindowMouse(hWnd,evMouseRightUp,pos.x,pos.y,GetModifiers());
        }
        break;
	case WM_CONTEXTMENU:
			if (pData->enabled)
			{
				POINT pos;

				pos.x = GET_X_LPARAM(lParam);
				pos.y = GET_Y_LPARAM(lParam);

				if (pos.x != 1 || pos.y != 1)
					ScreenToClient(hWnd, &pos);

				pos.x += pData->Origin.x;
				pos.y += pData->Origin.y;

				handleWindowContextMenu(hWnd,pos.x,pos.y,GetModifiers());
			}
        break;
	case WM_SYSKEYDOWN:
    case WM_KEYDOWN:
    	if (pData->enabled)
        {
            int c = CheckVirtualKeyCode ((int) wParam);

            if (!c) /* Ignore non-virtual keys, because they arrive as WM_SYSCHAR and WM_CHAR. */
                break;

            /* Handle virtual keys analogously to keys received as WM_SYSCHAR and WM_CHAR. */
            if (gInKey)
            {
                if (gCurChar == c)
                    handleWindowKeyboard(hWnd, evKeyStillDown, gCurChar, GetModifiers());
                else
                {
                    handleWindowKeyboard(hWnd, evKeyUp, gCurChar, GetModifiers());
                    gCurChar = c;
                    handleWindowKeyboard(hWnd, evKeyDown, gCurChar, GetModifiers());
                }
            }
            else
            {
                gCurChar = c;
                handleWindowKeyboard(hWnd, evKeyDown, gCurChar, GetModifiers());
                gInKey = TRUE;
            }
        }
        break;
	case WM_SYSCHAR:
    case WM_CHAR:
    	if (pData->enabled)
        {
            if (GetKeyState(VK_MENU) < 0)
                wParam += 256;

            if (gInKey)
            {
                if (gCurChar == (int) wParam)
                    handleWindowKeyboard(hWnd, evKeyStillDown, gCurChar, 0);
                else
                {
                    handleWindowKeyboard(hWnd, evKeyUp, gCurChar, 0);
                    gCurChar = wParam;
                    handleWindowKeyboard(hWnd, evKeyDown, gCurChar, 0);
                }
            }
            else
            {
                gCurChar = wParam;
                handleWindowKeyboard(hWnd, evKeyDown, gCurChar, 0);
                gInKey = TRUE;
            }
        }
        break;
    case WM_SYSKEYUP:
    case WM_KEYUP:
    	if (pData->enabled)
    	{
			if (gInKey)
				handleWindowKeyboard(hWnd, evKeyUp, gCurChar, GetModifiers());
			gInKey = FALSE;
			gCurChar = 0;
		}
        break;
    case WM_CTLCOLORSTATIC:
        {
            HDC hDC = (HDC) wParam;

            while (hWnd && !pData->hBackBrush)
            {
            	hWnd = GetParent(hWnd);
            	pData = (WindowData *) GetWindowLongPtr(hWnd,GWLP_USERDATA);
            }

            SelectObject(hDC, pData->hBackBrush);
            SetBkColor(hDC, pData->backColor);
            SetTextColor(hDC, pData->foreColor);
        }
        return (LRESULT) pData->hBackBrush;
	case WM_DRAWITEM:
		{
			LPDRAWITEMSTRUCT lpDIS = (LPDRAWITEMSTRUCT) lParam;

			if (lpDIS->CtlType == ODT_LISTBOX)
				DrawCheckListBoxItem(lpDIS);
		}
    }

	return CallWindowProc(pDefWindowProc, hWnd, uMsg, wParam, lParam);
};


LRESULT CALLBACK HSDIWindowFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

	switch (uMsg)
	{
	case WM_SETTEXT:
		{
			LPWSTR s = (LPWSTR) lParam;

			if (pFrameData->lpszAppName && *pFrameData->lpszAppName)
			{
				if (s && *s)
				{
					LPWSTR title;
					int nTextLen;

					nTextLen = wcslen(s);
					title = malloc((wcslen(pFrameData->lpszAppName)+nTextLen+6)*sizeof(wchar_t));

					if (title)
					{
						wcscpy(title, pFrameData->lpszAppName);
						wcscat(title, L" - [");
						wcscat(title, s);
						wcscat(title, L"]");
						SetWindowTextW(ghWndFrame, title);
					}

					free(title);
				}
				else
					SetWindowTextW(ghWndFrame, pFrameData->lpszAppName);
			}
			else
				SetWindowTextW(ghWndFrame, s ? s : "");
		}
		break;
	case WM_LBUTTONDOWN:
		SetActiveWindow(hWnd);
		SetFocus(hWnd);
		break;
    case WM_DESTROY:
    	pFrameData->hClientWnd = NULL;
		SetWindowTextW(ghWndFrame, pFrameData->lpszAppName ? pFrameData->lpszAppName : L"");
    	break;
    case WM_GETDLGCODE:
    	return DLGC_WANTMESSAGE;
	}

  	return HWindowSharedFunction(DefWindowProc, hWnd, uMsg, wParam, lParam );
}

LRESULT CALLBACK HDialogFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	WindowData *pData = (WindowData *) GetWindowLongPtr(hWnd,GWLP_USERDATA);

	switch (uMsg)
	{
	case WM_DESTROY:
		{
			HWND hOwner = GetWindow(hWnd, GW_OWNER);
			EnableWindow(hOwner, TRUE);
			SetActiveWindow(hOwner);
		}
		break;
	case WM_LBUTTONDOWN:
		SetActiveWindow(hWnd);
		break;
	case WM_ACTIVATE:
		if (wParam == WA_INACTIVE)
		{
			if (gInKey)
				handleWindowKeyboard(hWnd, evKeyLost, gCurChar, GetModifiers());
			gInKey = FALSE;
			gCurChar = 0;
			handleWindowDeactivate(hWnd);
		}
		else
		{
			handleWindowActivate(hWnd);
		}
		break;
	case WM_GETMINMAXINFO:
		if (pData)
		{
			MINMAXINFO *pInfo = (MINMAXINFO *) lParam;
			pInfo->ptMinTrackSize.x = pData->MinTrackSize.cx;
			pInfo->ptMinTrackSize.y = pData->MinTrackSize.cy;
			return 0;
		}
		break;
	}

	return HWindowSharedFunction(DefDlgProc, hWnd, uMsg, wParam, lParam);
}

LRESULT CALLBACK HMDIWindowFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

	switch (uMsg)
	{
	case WM_MDIACTIVATE:
		if (hWnd == (HWND) wParam)
			handleWindowDeactivate(hWnd);
		if (hWnd == (HWND) lParam)
			handleWindowActivate(hWnd);
		break;
	case WM_LBUTTONDOWN:
		SendMessage(pFrameData->hClientWnd,WM_MDIACTIVATE,(WPARAM) hWnd,0);
		break;
	};

  	return HWindowSharedFunction(DefMDIChildProc, hWnd, uMsg, wParam, lParam );
}

LRESULT CALLBACK HCompoundControlFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return HWindowSharedFunction(DefWindowProc, hWnd, uMsg, wParam, lParam);
}

void osInvalidateWindow(WindowHandle window)
{
    InvalidateRect(window,NULL,FALSE);
};

void osInvalidateWindowRect(WindowHandle window, int left, int top, int right, int bottom)
{
    RECT rect;
    rect.left   = left;
    rect.top    = top;
    rect.right  = right;
    rect.bottom = bottom;
    InvalidateRect(window,&rect,FALSE);
}

WindowHandle osCreateWindow()
{
    HWND hWnd;
    RECT rect;
    FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

	switch (pFrameData->DocumentInterface)
	{
	case 1: // SDI
		if (pFrameData->hClientWnd)  // There is already created window and SDI does't allow
			return NULL;			 //more that one window at the same time.

		GetClientRect(ghWndFrame, &rect);
		hWnd = CreateWindowW(
				  L"HSDIWINDOW",
				  NULL,
				  WS_CHILD | WS_HSCROLL | WS_VSCROLL,
				  0,0,0,0,
				  ghWndFrame,
				  NULL,
				  ghModule,
				  NULL
				);
		pFrameData->hClientWnd = hWnd;

		RelayoutFrameBars();
		break;
    case 2: // MDI
    	{
			MDICREATESTRUCT mdicreate;		// The structure sent to the client window

			/* fill the MDICREATESTRUCT record */
			mdicreate.szClass = "HMDIWINDOW";
			mdicreate.szTitle = NULL;
			mdicreate.hOwner  = ghModule;
			mdicreate.x       = 0;
			mdicreate.y       = 0;
			mdicreate.cx      = 0;
			mdicreate.cy      = 0;
			mdicreate.style   = WS_OVERLAPPEDWINDOW | WS_HSCROLL | WS_VSCROLL;
			mdicreate.lParam  = 0;

			/* create the window */
			hWnd = (HWND) SendMessageW(pFrameData->hClientWnd,WM_MDICREATE,0,(LPARAM) &mdicreate);
		}
		break;
	}

	return hWnd;
};

WindowHandle osCreateDialog(WindowHandle parent)
{
    char *s;
    WORD *p;
    WORD dlgtemplate[60];
    DWORD lStyle;
    HWND hDlg;

    // start to fill in the dlgtemplate information. Addressing by WORDs
    lStyle = WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | DS_CENTER | DS_MODALFRAME;

    p = dlgtemplate;
    *p++ = LOWORD (lStyle);
    *p++ = HIWORD (lStyle);
    *p++ = 0;       // LOWORD (lExtendedStyle)
    *p++ = 0;       // HIWORD (lExtendedStyle)
    *p++ = 0;       // NumberOfItems
    *p++ = 0;       // x  (dummy value)
    *p++ = 0;       // y  (dummy value)
    *p++ = 0;       // cx (dummy value)
    *p++ = 0;       // cy (dummy value)
    *p++ = 0;       // Menu
    s = "HDIALOG"; do { *p++ = (WORD) *s; } while (*s++);   // Class
    *p++ = 0;       // Empty title
    // Font information because of DS_SETFONT
    *p++ = 8;       // point size
    s = "MS Sans Serif"; do { *p++ = (WORD) *s; } while (*s++);

    if (!parent) parent = ghWndFrame;

    hDlg = CreateDialogIndirectParam (ghModule, (LPCDLGTEMPLATE) dlgtemplate, parent, (DLGPROC) NULL, (LPARAM) 0);
    if (!hDlg) return NULL;

    ShowWindow(hDlg, SW_HIDE);

    return hDlg;
};

WindowHandle osCreateCompoundControl(WindowHandle form)
{
    HWND hWnd;

	hWnd = CreateWindowW(
				L"HCOMPOUND",
				NULL,
				WS_CHILD | WS_BORDER | WS_HSCROLL | WS_VSCROLL,
				0,0,0,0,
				form,
				NULL,
				ghModule,
				NULL
				);
	return checkWindow(hWnd, "HCOMPOUND");
};

void osGetCompoundControlReqSize(WindowHandle listbox, int *res)
{
	res[0] = 10;
	res[1] = 10;
}

void osSetWindowColor(WindowHandle window, int foreColor, int backColor, int hatchStyle, BitmapHandle patBmp)
{
	LOGBRUSH lb;
	WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

	if (pData->backColor != backColor ||
		pData->hatchStyle != hatchStyle ||
		pData->patBmp != patBmp)
	{
		if (pData->hBackBrush) DeleteObject(pData->hBackBrush);

		lb.lbColor = backColor;
		SetupLogBrush(&lb, FALSE, pData->hatchStyle, pData->patBmp);
		pData->hBackBrush = CreateBrushIndirect(&lb);
	}

	pData->backColor = backColor;
	pData->foreColor = foreColor;
}

PortString osGetWindowTitle(WindowHandle window)
{
    int nLen = GetWindowTextLengthW(window);
    PortString buffer = (PortString) rmalloc((nLen+1)*sizeof(wchar_t));
    GetWindowTextW(window, buffer, nLen+1);
    return buffer;
};

void osSetWindowTitle(WindowHandle window, PortString txt)
{
    SetWindowTextW(window, txt);
};

void osGetWindowViewSize(WindowHandle window, int *res)
{
    RECT rect;
    GetClientRect(window,&rect);

    res[0] = rect.right-rect.left;
    res[1] = rect.bottom-rect.top;
}

void osSetWindowViewSize(WindowHandle window, int w, int h)
{
	char buffer[20];
	HWND hTargetWnd;
    RECT crect, wrect;

    GetClassName(window,buffer,sizeof(buffer));
	hTargetWnd = (_stricmp(buffer, "HSDIWINDOW") == 0) ? ghWndFrame : window;

    GetClientRect(hTargetWnd,&crect);
    GetWindowRect(hTargetWnd,&wrect);

    MoveWindow(hTargetWnd,wrect.left,wrect.top,
        (wrect.right-wrect.left) + (w - (crect.right-crect.left)),
        (wrect.bottom-wrect.top) + (h - (crect.bottom - crect.top)),
        TRUE);
}

void osSetWindowDomainSize(WindowHandle window, int cx, int cy)
{
    RECT rect;
    SCROLLINFO si;
    int  nWidth, nHeight;
    int  nLimitX, nLimitY;
    WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

    pData->DomainSize.cx = cx;
    pData->DomainSize.cy = cy;

    GetClientRect(window,&rect);
    nWidth  = rect.right -rect.left;
    nHeight = rect.bottom-rect.top;

    nLimitX = cx - nWidth;
    nLimitY = cy - nHeight;

    si.cbSize = sizeof(si);
    si.fMask  = SIF_PAGE | SIF_RANGE;

    si.nMin = 0;
    si.nMax = pData->DomainSize.cx-1;
    si.nPage = nWidth;
    SetScrollInfo(window, SB_HORZ, &si, TRUE);

    si.nMin = 0;
    si.nMax = pData->DomainSize.cy-1;
    si.nPage = nHeight;
    SetScrollInfo(window, SB_VERT, &si, TRUE);

    if (nLimitX > 0 && pData->Origin.x > nLimitX) pData->Origin.x = nLimitX;
    if (nLimitY > 0 && pData->Origin.y > nLimitY) pData->Origin.y = nLimitY;
}

void osSetWindowScrollOrigin(WindowHandle window, int x, int y)
{
    WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

    SetScrollPos(window,SB_HORZ,x,TRUE);
    SetScrollPos(window,SB_VERT,y,TRUE);
    moveControls(window, pData->Origin.x-x, pData->Origin.y-y);
    InvalidateRect(window,NULL,TRUE);
    pData->Origin.x = x;
    pData->Origin.y = y;
}

void osGetWindowScrollOrigin(WindowHandle window, int *res)
{
    WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

    res[0] = pData->Origin.x;
    res[1] = pData->Origin.y;
}

void osSetWindowLineSize(WindowHandle window, int cx, int cy)
{
    WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

    pData->LineSize.cx = cx;
    pData->LineSize.cy = cy;
}

void osGetWindowLineSize(WindowHandle window, int *res)
{
    WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

    res[0] = pData->LineSize.cx;
    res[1] = pData->LineSize.cy;
}

void osSetWindowPageSize(WindowHandle window, int cx, int cy)
{
    WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

    pData->PageSize.cx = cx;
    pData->PageSize.cy = cy;
}

void osGetWindowPageSize(WindowHandle window, int *res)
{
    WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

    res[0] = pData->PageSize.cx;
    res[1] = pData->PageSize.cy;
}

void osSetWindowVisible(WindowHandle window, BOOL visible)
{
	if (visible)
	{
		SetupWindowPosition(window);
    	ShowWindow(window, SW_SHOW);
	}
	else
		ShowWindow(window, SW_HIDE);
}

BOOL osGetWindowVisible(WindowHandle window)
{
    return IsWindowVisible(window);
}

void osRunDialog(WindowHandle window)
{
    MSG msg;

    osSetWindowVisible(window, TRUE);
    SetActiveWindow(window);
    EnableWindow(GetWindow(window, GW_OWNER), FALSE);

    while (IsWindow(window) && GetMessage(&msg, NULL, 0, 0) != 0)
    {
		if (IsDialogMessage(window, &msg))
			continue;

       	TranslateMessage(&msg);
		DispatchMessage(&msg);
    };
}

BOOL osDismissWindow(WindowHandle window)
{
	SendMessage(window, WM_CLOSE, 0, 0);
	return !IsWindow(window);
}

void osDestroyWindow(WindowHandle window)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	if (pFrameData->DocumentInterface == 1 || GetParent(window) != pFrameData->hClientWnd)
	{
		HWND hOwner = GetWindow(window, GW_OWNER);
		if (hOwner)
		{
			EnableWindow(hOwner, TRUE);
			SetActiveWindow(hOwner);
		}

		DestroyWindow(window);
	}
	else
		SendMessage(pFrameData->hClientWnd,WM_MDIDESTROY,(WPARAM) window,0);
}

void osSetWindowEnabled(WindowHandle window, BOOL enabled)
{
	int i;
	HWND hCtrl;
	WindowData *pData;

	pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

	if (pData->enabled != enabled)
	{
		pData->enabled = enabled;

		if (enabled)
		{
			hCtrl = GetTopWindow(window);
			while (hCtrl)
			{
				if (pData->disabledCtrlsCount == 0)
					EnableWindow(hCtrl, TRUE);
				else
				{
					pData->disabledCtrls[pData->disabledCtrlsCount] = hCtrl;
					for (i = 0; pData->disabledCtrls[i] != hCtrl; i++);
					EnableWindow(hCtrl, i >= pData->disabledCtrlsCount);
				}

				hCtrl = GetNextWindow(hCtrl,GW_HWNDNEXT);
			}
			free(pData->disabledCtrls);
			pData->disabledCtrls = NULL;
			pData->disabledCtrlsCount = 0;
		}
		else
		{
			hCtrl = GetTopWindow(window);
			while (hCtrl)
			{
				if (!IsWindowEnabled(hCtrl))
				{
					if (pData->disabledCtrlsCount % 8 == 0)
					{
						pData->disabledCtrls = realloc(pData->disabledCtrls, (pData->disabledCtrlsCount+9)*sizeof(HWND));
					}

					pData->disabledCtrls[pData->disabledCtrlsCount++] = hCtrl;
				}

				EnableWindow(hCtrl, FALSE);
				hCtrl = GetNextWindow(hCtrl,GW_HWNDNEXT);
			}
		}
	}
}

BOOL osGetWindowEnabled(WindowHandle window)
{
	WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);
	return pData->enabled;
}

CanvasHandle osGetWindowCanvas(WindowHandle window)
{
    HDC hDC;
    HRGN theRegion;
    RECT rect;
    WindowData *pData;
    CanvasHandle canvas;

    pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);
    if (pData==NULL)
    {
        printf("Invalid window handle\n");
        exit(1);
    }

    hDC = GetDC(window);
    if (hDC==NULL)
    {
        printf("GetDC returned NULL\n");
        exit(1);
    }

    SetViewportExtEx(hDC,pData->DomainSize.cx,pData->DomainSize.cy,NULL);
    SetWindowExtEx(hDC,pData->DomainSize.cx,pData->DomainSize.cy,NULL);
    SetViewportOrgEx(hDC,-pData->Origin.x,-pData->Origin.y,NULL);

    GetClientRect(window, &rect);

    theRegion = CreateRectRgn(rect.left, rect.top, rect.right, rect.bottom);
    getWindowClipRgn(window, theRegion);
    SelectClipRgn (hDC, theRegion);
    DeleteObject(theRegion);

    canvas = rmalloc(sizeof(*canvas));
    memset(canvas, 0, sizeof(*canvas));
    canvas->hDC = hDC;
    canvas->bInvalidated = FALSE;
    return canvas;
}

void osReleaseWindowCanvas(WindowHandle window, CanvasHandle canvas)
{
    if (!ReleaseDC (window, canvas->hDC))
    {
        printf("ReleaseDC returned zero\n");
    }
    rfree(canvas);
}   /* osReleaseWindowCanvas */

void osMoveResizeControl(WindowHandle ctrl, int x, int y, int w, int h)
{
    char buffer[20];
    HWND hWnd = GetParent(ctrl);
    WindowData *pData = (WindowData *) GetWindowLongPtr(hWnd,GWLP_USERDATA);

    GetClassName(ctrl,buffer,sizeof(buffer));
    if (_stricmp(buffer, "ComboBox") == 0) h = 150;

    MoveWindow(ctrl,x-pData->Origin.x,y-pData->Origin.y,w,h,TRUE);
    InvalidateRect(ctrl,NULL,TRUE);
}

void osGetControlRect(WindowHandle ctrl, int *res)
{
    HWND hWnd = GetParent(ctrl);
    WindowData *pData = (WindowData *) GetWindowLongPtr(hWnd,GWLP_USERDATA);

    RECT rect;
    GetWindowRect(ctrl,&rect);
    res[0] = rect.left+pData->Origin.x;
    res[1] = rect.top+pData->Origin.y;
    res[2] = rect.right+pData->Origin.x;
    res[3] = rect.bottom+pData->Origin.y;
}

void osSetControlEnabled(WindowHandle ctrl, BOOL enabled)
{
	int i;
	WindowData *pData = (WindowData *) GetWindowLongPtr(GetParent(ctrl),GWLP_USERDATA);

	if (pData->enabled)
		EnableWindow(ctrl, enabled);
	else
	{
		if (enabled)
		{
			pData->disabledCtrls[pData->disabledCtrlsCount] = ctrl;
			for (i = 0; pData->disabledCtrls[i] != ctrl; i++);

			if (i < pData->disabledCtrlsCount)
			{
				pData->disabledCtrlsCount--;
				memmove(pData->disabledCtrls+i, pData->disabledCtrls+i+1, (pData->disabledCtrlsCount-i)*sizeof(HWND));
			}
		}
		else
		{
			if (pData->disabledCtrlsCount % 8 == 0)
			{
				pData->disabledCtrls = realloc(pData->disabledCtrls, (pData->disabledCtrlsCount+9)*sizeof(HWND));
			}

			pData->disabledCtrls[pData->disabledCtrlsCount++] = ctrl;
		}
	}
}

BOOL osGetControlEnabled(WindowHandle ctrl)
{
	int i;
	WindowData *pData = (WindowData *) GetWindowLongPtr(GetParent(ctrl),GWLP_USERDATA);

	if (pData->enabled)
		return IsWindowEnabled(ctrl);
	else
	{
		pData->disabledCtrls[pData->disabledCtrlsCount] = ctrl;
		for (i = 0; pData->disabledCtrls[i] != ctrl; i++);

		return (i >= pData->disabledCtrlsCount);
	}
}

void osSetControlVisible(WindowHandle ctrl, BOOL visible)
{
	RECT rect;
	HWND hParent;
	LONG lStyle;

	lStyle = GetWindowLong(ctrl, GWL_STYLE);
	lStyle = visible ? (lStyle | WS_VISIBLE) : (lStyle & ~WS_VISIBLE);
	SetWindowLong(ctrl, GWL_STYLE, lStyle);

	hParent = GetParent(ctrl);

	GetWindowRect(ctrl, &rect);
	MapWindowPoints(NULL, hParent, (LPPOINT) &rect, 2);
	InvalidateRect(hParent,&rect,FALSE);
}

BOOL osGetControlVisible(WindowHandle ctrl)
{
	return IsWindowVisible(ctrl);
}

void osSetControlTip(WindowHandle ctrl, PortString text)
{
	TOOLINFO ti;					/* The tool information that is sent to the tooltip control. */
	HWND hParent;
	WindowData *pData;

	hParent = GetParent(ctrl);
	pData = (WindowData *) GetWindowLongPtr(GetParent(ctrl),GWLP_USERDATA);

	if (!pData->hTooltip)
	{
		pData->hTooltip = CreateWindowExW(WS_EX_TOPMOST,   // Apply the topmost style for this window
		      TOOLTIPS_CLASSW,                             // Class name
		      NULL,                                        // Title (NULL)
		      WS_POPUP | TTS_ALWAYSTIP,                    // Style *must* be WS_POPUP
		      CW_USEDEFAULT,                               // Default position (x,y)
		      CW_USEDEFAULT,
		      CW_USEDEFAULT,                               // Default size (w,h)
		      CW_USEDEFAULT,
		      hParent,                                     // Parent
		      (HMENU) NULL,                                // No menu
		      (HANDLE) ghModule,                           // The instance
		      NULL                                         // No window creation data
		      );
		SendMessageW(pData->hTooltip, TTM_SETMAXTIPWIDTH, 0, 400); // Enable multiline tooltips
	}

	if (text && *text)
	{
		if (wcslen(text) > 255)
			text[256] = 0; //  Truncate the tip text to 255 chars because the osGetControlTip function
			               //uses 256 bytes long buffer

		/* Fill the tooltip info with the appropriate information. */
		ti.cbSize     = sizeof(TOOLINFO);
		ti.uFlags     = TTF_IDISHWND | TTF_SUBCLASS;
		ti.hwnd       = hParent;
		ti.uId        = (UINT_PTR) ctrl;
		ti.rect.left  = 0;
		ti.rect.top   = 0;
		ti.rect.right = 0;
		ti.rect.bottom= 0;
		ti.hinst      = ghModule;
		ti.lpszText   = (char*) text;

		SendMessageW(pData->hTooltip, TTM_ADDTOOL, 0, (LPARAM) (LPTOOLINFO)&ti);
	}
	else
	{
		/* Fill the tooltip info with the appropriate information. */
		ti.cbSize     = sizeof(TOOLINFO);
		ti.uFlags     = TTF_IDISHWND;
		ti.hwnd       = hParent;
		ti.uId        = (UINT_PTR) ctrl;

		SendMessage (pData->hTooltip, TTM_DELTOOL, 0, (LPARAM) (LPTOOLINFO)&ti);
	}
}

PortString osGetControlTip(WindowHandle ctrl)
{
	TOOLINFO ti;					/* The tool information that is sent to the tooltip control. */
	HWND hParent;
	WindowData *pData;

	hParent = GetParent(ctrl);
	pData = (WindowData *) GetWindowLongPtr(GetParent(ctrl),GWLP_USERDATA);

	if (pData->hTooltip)
	{
		/* Fill the tooltip info with the appropriate information. */
		ti.cbSize     = sizeof(TOOLINFO);

		ti.uFlags     = TTF_IDISHWND;
		ti.hwnd       = hParent;
		ti.uId        = (UINT_PTR) ctrl;
		ti.lpszText   = malloc(512);

		if (ti.lpszText)
		{
			*ti.lpszText  = 0;
			SendMessageW (pData->hTooltip, TTM_GETTEXT, 0, (LPARAM) (LPTOOLINFO)&ti);

			if (*ti.lpszText == 0)
			{
				free(ti.lpszText);
				ti.lpszText = NULL;
			}
		}

		return (PortString) ti.lpszText;
	}
	else
		return NULL;
}

void osSetWindowPosition(WindowHandle window, int position, int x0, int y0, int x1, int y1)
{
	char buffer[20];

	GetClassName(window,buffer,sizeof(buffer));

	if (_stricmp(buffer, "HSDIWINDOW") == 0)
	{
		POINT pos;
		SIZE sz;

		pos.x = x0;
		pos.y = y0;
		sz.cx = x1-x0;
		sz.cy = y1-y0;

		switch (position)
		{
		// When the window is a SDI window then the WinPosCenterToParent is the same as WinPosCenter
		case 1:
		case 2:
			CenterToScreen(&pos, sz);
			break;
		case 3:
			GetCursorPos(&pos);
			break;
		}

		MoveWindow(ghWndFrame,pos.x,pos.y,sz.cx,sz.cy,TRUE);
	}
	else
	{
		WindowData *pData = (WindowData *) GetWindowLongPtr(window,GWLP_USERDATA);

		// if the window is a child window then the WinPosCenter is the same as WinPosCenterToParent
		if (GetParent(window) && position == 1)
			pData->windowPos = 2;
		else
			pData->windowPos = position;

		pData->windowPosRect.left   = x0;
		pData->windowPosRect.top    = y0;
		pData->windowPosRect.right  = x1;
		pData->windowPosRect.bottom = y1;

		if (IsWindowVisible(window))
			SetupWindowPosition(window);
	}
}

void osGetWindowRect(WindowHandle ctrl, int *res)
{
    RECT rect;
    GetWindowRect(ctrl,&rect);
    res[0] = rect.left;
    res[1] = rect.top;
    res[2] = rect.right;
    res[3] = rect.bottom;
}

void osSetWindowResizeable(WindowHandle hwnd, int resizeable)
{
  RECT rc;
  LONG style;
  HWND hTargetWnd;
  FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

  hTargetWnd = hwnd;
  if (pFrameData->DocumentInterface == 1 && GetParent(hwnd) == ghWndFrame)
  	hTargetWnd = ghWndFrame;

  style = GetWindowLong(hTargetWnd, GWL_STYLE);
  if (resizeable)
  	if (style & DS_MODALFRAME)
  		style |= WS_THICKFRAME;
  	else
  		style |= WS_THICKFRAME | WS_MAXIMIZEBOX;
  else
    style &= ~(WS_THICKFRAME | WS_MAXIMIZEBOX);
  SetWindowLong(hTargetWnd, GWL_STYLE, style);

  GetWindowRect(hTargetWnd,&rc);
  SetWindowPos(hTargetWnd,NULL,rc.left,rc.top,rc.right-rc.left,rc.bottom-rc.top,SWP_NOZORDER | SWP_FRAMECHANGED);

  GetClientRect(hwnd,&rc);
  handleWindowResize(hwnd,rc.right-rc.left,rc.bottom-rc.top);
  handleContainerReLayout(hwnd);

  if (GetParent(hwnd) == NULL)
  {
	rc.left = rc.right-14;
	rc.top  = rc.bottom-14;
	InvalidateRect(hwnd,&rc,TRUE);
  }
}

void osReLayoutContainer(WindowHandle window)
{
	char buffer[20];
	GetClassName(window,buffer,sizeof(buffer));

	if (_stricmp(buffer, "HNOTEBOOK")     == 0 ||
		_stricmp(buffer, "HNOTEBOOKPAGE") == 0 ||
		_stricmp(buffer, "HGROUPBOX")     == 0)
	{
		osForceContainerReLayout(window);
	}

	handleContainerReLayout(window);
}

void osForceContainerReLayout(HWND hCtrl)
{
	char buffer[20];
	HWND hWnd = hCtrl;
	while (hWnd)
	{
		hWnd = GetParent(hWnd);
		GetClassName(hWnd,buffer,sizeof(buffer));

		if (_stricmp(buffer, "HNOTEBOOK") != 0 &&
		    _stricmp(buffer, "HNOTEBOOKPAGE") != 0 &&
		    _stricmp(buffer, "HGROUPBOX") != 0)
		{
			handleContainerReLayout(hWnd);
			break;
		}
	}
}

void osSetDialogMinSize(WindowHandle dialog, int w, int h)
{
	RECT rect;
	WindowData *pData = (WindowData *) GetWindowLongPtr(dialog,GWLP_USERDATA);

	rect.left   = 0;
	rect.top    = 0;
	rect.right  = w;
	rect.bottom = h;
	AdjustWindowRect(&rect, GetWindowLong(dialog, GWL_STYLE), FALSE);

	pData->MinTrackSize.cx = rect.right-rect.left;
	pData->MinTrackSize.cy = rect.bottom-rect.top;
}
