#include "Types.h"
#include "Window.h"
#include "DockBar.h"
#include "StatusBar.h"
#include "Action.h"
#include "ActionsMap.h"
#include "Canvas.h"
#include "ConfigKey.h"
#include "Internals.h"
#include "Handlers_stub.h"

#define OSMenuIDEnd 1500

#define Spacing 1

static void BuildAccelString(ActionHandle action, char *text)
{
	*text = 0;

	if (!action || !action->key)
		return;

	if (action->keyMods & altBIT  ) strcat(text, "Alt+");
	if (action->keyMods & ctrlBIT ) strcat(text, "Ctrl+");
	if (action->keyMods & shiftBIT) strcat(text, "Shift+");

	switch (action->key)
	{
	case kbBackSpace: 	strcat(text, "BackSpace");	break;
	case kbTab: 		strcat(text, "Tab");		break;
	case kbEnter: 		strcat(text, "Enter");		break;
	case kbEscape:		strcat(text, "Esc");		break;
	case kbBegin: 		strcat(text, "Home");		break;
	case kbClear: 		strcat(text, "Clear");		break;
	case kbDelete:		strcat(text, "Del");		break;
	case kbDown: 		strcat(text, "Down");		break;
	case kbEnd: 		strcat(text, "End");		break;
	case kbF1: 			strcat(text, "F1");			break;
	case kbF2: 			strcat(text, "F2");			break;
	case kbF3: 			strcat(text, "F3");			break;
	case kbF4: 			strcat(text, "F4");			break;
	case kbF5: 			strcat(text, "F5");			break;
	case kbF6: 			strcat(text, "F6");			break;
	case kbF7: 			strcat(text, "F7");			break;
	case kbF8: 			strcat(text, "F8");			break;
	case kbF9: 			strcat(text, "F9");			break;
	case kbF10:			strcat(text, "F10");		break;
	case kbF11:			strcat(text, "F11");		break;
	case kbF12:			strcat(text, "F12");		break;
	case kbF13:			strcat(text, "F13");		break;
	case kbF14:			strcat(text, "F14");		break;
	case kbF15:			strcat(text, "F15");		break;
	case kbHelp: 		strcat(text, "Help");		break;
	case kbLeft: 		strcat(text, "Left");		break;
	case kbPgDown: 		strcat(text, "PgDown");		break;
	case kbPgUp: 		strcat(text, "PgUp");		break;
	case kbRight: 		strcat(text, "Right");		break;
	case kbUp: 			strcat(text, "Up");			break;
	case ' ':			strcat(text, "Space");		break;
	default:
		{
			char s[2];

			if (action->key >= 256)
			{
				strcat(text,"Alt+");
				s[0] = action->key-256;
			}

			if (iscntrl(action->key))
			{
				s[0] = '@'+action->key;
				strcat(text,"Ctrl+");
			}
			else
				if (isupper(action->key)) strcat(text,"Shift+");
				else s[0] = toupper(action->key);

			s[1] = 0;
			strcat(text, s);
		}
		break;
	}
}

static void DrawMenuText(HDC hdc, ActionHandle action, LPRECT lpRect)
{
	char szAccText[32];
	BuildAccelString(action, szAccText);

	DrawText(hdc, action->title, -1, lpRect, DT_SINGLELINE|DT_VCENTER|DT_LEFT);
	DrawText(hdc, szAccText,     -1, lpRect, DT_SINGLELINE|DT_VCENTER|DT_RIGHT);
}

void DrawMenuItemCheck(HDC hdc, ActionHandle action, RECT rcFrame, BOOL bSelected)
{
	HBITMAP    bmAndObject, bmAndBack, bmAndMem;
	HBITMAP    bmObjectOld, bmBackOld, bmMemOld;
	HDC        hdcObj, hdcBack, hdcMem;
	POINT      ptSize;
	RECT       rc;

	ptSize.x = rcFrame.right  - rcFrame.left;
	ptSize.y = rcFrame.bottom - rcFrame.top;

	rc.left  = 0;
	rc.top   = 0;
	rc.right = ptSize.x;
	rc.bottom= ptSize.y;

	// Create some DCs to hold temporary data.
	hdcMem  = CreateCompatibleDC(hdc);
	hdcBack = CreateCompatibleDC(hdc);	
	hdcObj  = CreateCompatibleDC(hdc);	

	// Create a bitmap for each DC. DCs are required for a number of
	// GDI functions.

	// Monochrome DC
	bmAndObject = CreateBitmap(ptSize.x, ptSize.y, 1, 1, NULL);
	bmAndBack   = CreateBitmap(ptSize.x, ptSize.y, 1, 1, NULL);

	bmAndMem    = CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

	// Each DC must select a bitmap object to store pixel data.	
	bmObjectOld = SelectObject(hdcObj,  bmAndObject);
	bmBackOld   = SelectObject(hdcBack, bmAndBack);
	bmMemOld    = SelectObject(hdcMem,  bmAndMem);

	// Set proper mapping mode.
	SetMapMode(hdcObj, GetMapMode(hdc));

	switch (action->type)
	{
	case ACTION_CHECK:
		DrawFrameControl(hdcObj, &rc, DFC_MENU, DFCS_MENUCHECK);
		break;
	case ACTION_RADIO:
		DrawFrameControl(hdcObj, &rc, DFC_MENU, DFCS_MENUBULLET);
		break;
	}

	// Create the inverse of the object.
	BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObj, 0, 0, NOTSRCCOPY);

	// Copy the background of the main DC to the destination.
	BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, rcFrame.left, rcFrame.top, SRCCOPY);

	// Mask out the places where the bitmap will be placed.
	BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObj, 0, 0, SRCAND);

	if (bSelected)
	{
		// XOR the bitmap with the background on the destination DC.
		BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCPAINT);
	}

	// Copy the destination to the screen.
	BitBlt(hdc, rcFrame.left, rcFrame.top, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);

	// Delete the memory bitmaps.
	DeleteObject(SelectObject(hdcMem,  bmMemOld));	
	DeleteObject(SelectObject(hdcBack, bmBackOld));
	DeleteObject(SelectObject(hdcObj,  bmObjectOld));	

	// Delete the memory DCs.
	DeleteDC(hdcMem);
	DeleteDC(hdcBack);
	DeleteDC(hdcObj);
}

void SaveWindowState(HWND hWnd)
{
	PortString keyName;
    WINDOWPLACEMENT wp;
    wp.length = sizeof(wp);

    if (GetWindowPlacement(hWnd, &wp))
    {
        if (IsIconic(hWnd))
          // never restore to Iconic state
          wp.showCmd = SW_SHOW;

        if ((wp.flags & WPF_RESTORETOMAXIMIZED) != 0)
          // if maximized and maybe iconic restore maximized state
          wp.showCmd = SW_SHOWMAXIMIZED;

        // and write it to the registry
		keyName = psdup(L"HToolkit.FrameState");
		osSetConfigIntKey(keyName, wp.showCmd);
		free(keyName);
    }
}

void RestoreWindowState(HWND hWnd, int nShow)
{
	PortString keyName;

    keyName = psdup(L"HToolkit.FrameState");
	nShow = osGetConfigIntKey(keyName, nShow);
	free(keyName);

	ShowWindow(hWnd, nShow);
}

LRESULT CALLBACK HFrameSharedFunction(int DocumentInterface, HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	ActionHandle action;
	FrameData *pData = (FrameData *) GetWindowLongPtr(hWnd,GWLP_USERDATA);

	switch (uMsg)
	{
	case WM_CLOSE:
		SaveWindowState(hWnd); // save the current window placement to be restored the next time
		handleProcessDismiss();
		return 0;
	case WM_DESTROY:
		// destroy all popup menus
		while (pData->pActionsMap->popupMenus)
			osDestroyMenu(pData->pActionsMap->popupMenus);

		// destroy all menus from the menu bar
		while (pData->pActionsMap->menus)
			osDestroyMenu(pData->pActionsMap->menus);

		osDestroyAllActions();

		// clear StatusBar context stack
		while (pData->statusContexts)
		{
			StatusContext context = pData->statusContexts;
			pData->statusContexts = context->next;
			free(context);
		}

		// clear StatusBar indicators list
		while (pData->first_indicator)
		{
			IndicatorHandle indicator;

			indicator = pData->first_indicator;
			handleIndicatorDestroy(indicator);
			pData->first_indicator = pData->first_indicator->next;

			if (pData->first_indicator)
				pData->first_indicator->prev = NULL;

			free(indicator->title);
			free(indicator);
		}
		pData->last_indicator = NULL;

		handleProcessDestroy();

		free(pData->lpszAppName);
		free(pData->lpszAppVersion);
		deleteActionsMap(pData->pActionsMap);

		free(pData);
		PostQuitMessage(0);
		break;
	case WM_CREATE:
		{
			pData = (FrameData *) malloc(sizeof(FrameData));
			if (!pData) return -1;

			pData->hClientWnd = NULL;
			pData->DocumentInterface = DocumentInterface;
			pData->lpszAppName = NULL;
			pData->lpszAppVersion = NULL;
			pData->pActionsMap = newActionsMap();

			pData->hLeftBar  = CreateWindow("HDOCKBAR",
											NULL,
											WS_CHILD | WS_VISIBLE | CCS_VERT,
											0,0,0,0,
											hWnd,
											NULL,
											(HANDLE) ghModule,
											NULL);
			pData->hTopBar   = CreateWindow("HDOCKBAR",
											NULL,
											WS_CHILD | WS_VISIBLE,
											0,0,0,0,
											hWnd,
											NULL,
											(HANDLE) ghModule,
											NULL);
			pData->hRightBar = CreateWindow("HDOCKBAR",
											NULL,
											WS_CHILD | WS_VISIBLE | CCS_VERT,
											0,0,0,0,
											hWnd,
											NULL,
											(HANDLE) ghModule,
											NULL);
			pData->hBottomBar= CreateWindow("HDOCKBAR",
											NULL,
											WS_CHILD | WS_VISIBLE,
											0,0,0,0,
											hWnd,
											NULL,
											(HANDLE) ghModule,
											NULL);

			pData->hStatusBar = CreateWindow("msctls_statusbar32",
											NULL,
											WS_CHILD,
											0,0,0,0,
											hWnd,
											NULL,
											(HANDLE) ghModule,
											NULL);
			pData->statusContexts   = NULL;
			pData->first_indicator  = NULL;
			pData->last_indicator   = NULL;

			SetWindowLongPtr(hWnd,GWLP_USERDATA,(LONG_PTR) pData);
		}
		break;
	case WM_SIZE:
		RelayoutFrameBars();
		RefreshStatusBarIndicators();
		return 0;
	case WM_COMMAND:
		if (lParam == 0)
			osActivateAction(LOWORD(wParam));
        break;
    case WM_MEASUREITEM:
        {
            LPMEASUREITEMSTRUCT lpMIS = (LPMEASUREITEMSTRUCT) lParam;

            if (lpMIS->CtlType == ODT_MENU)
            {
				action = getActionHandle(pData->pActionsMap, lpMIS->itemID);

				if (action)
				{
            		NONCLIENTMETRICS ncm;
            		HFONT hFont, hOldFont;
            		RECT rc[2];
            		HDC hDC;
            		long lDims;
            		WORD wCheckWidth, wCheckHeight;
            		char szAccText[32];

					BuildAccelString(action, szAccText);

            		hDC = GetDC(hWnd);

					ZeroMemory(&ncm,sizeof(ncm));
					ncm.cbSize = sizeof(ncm);

					// Get the menu dimensions
					SystemParametersInfo(SPI_GETNONCLIENTMETRICS,0,(PVOID)&ncm,FALSE);

					// Create a font based on menu metrics
					hFont = CreateFontIndirect(&ncm.lfMenuFont);

					hOldFont = SelectObject(hDC, hFont);

					// Draw out menu item caption - text.
					ZeroMemory(&rc,sizeof(rc));
					DrawText(hDC,action->title,-1,&rc[0],DT_SINGLELINE|DT_VCENTER|DT_LEFT|DT_CALCRECT);
					DrawText(hDC,szAccText,    -1,&rc[1],DT_SINGLELINE|DT_VCENTER|DT_LEFT|DT_CALCRECT);

					rc[0].right += rc[1].right-rc[1].left;
					rc[0].bottom = max(rc[0].bottom, rc[1].bottom);

					lDims = GetMenuCheckMarkDimensions();
					wCheckWidth  = (WORD)(LOWORD(lDims)+Spacing);
					wCheckHeight = (WORD)(HIWORD(lDims)+Spacing);

					if (action->bitmap)
					{
						wCheckWidth  = (WORD)max(action->bitmap->destsize.cx, wCheckWidth);
						wCheckHeight = (WORD)max(action->bitmap->destsize.cy, wCheckHeight);
					}

					lpMIS->itemWidth  = (rc[0].right-rc[0].left) + wCheckWidth     + (Spacing*3);	// Text width
					lpMIS->itemHeight = max((rc[0].bottom-rc[0].top),wCheckHeight) + (Spacing*2); // Text Height


					// Clean up resources
					SelectObject(hDC,hOldFont);
					DeleteObject(hFont);
					ReleaseDC(hWnd,hDC);
				}
            }
        }
        break;
	case WM_DRAWITEM:
		{
			LPDRAWITEMSTRUCT lpDIS = (LPDRAWITEMSTRUCT) lParam;

			if (lpDIS->CtlType == ODT_MENU)
			{
				action = getActionHandle(pData->pActionsMap, lpDIS->itemID);

				if (action)
				{
					long lDims;
					RECT rcFrame, rcBox, rc;
					POINT pt;
					WORD wCheckWidth, wCheckHeight;
					WORD wWidth, wHeight;
					int nIndexDC;
					HBRUSH hFillBrush;
					struct CanvasHandle canvas;

					canvas.hDC = lpDIS->hDC;

					// Check box dimensions
					lDims = GetMenuCheckMarkDimensions();
					wCheckWidth  = LOWORD(lDims);
					wCheckHeight = HIWORD(lDims);

					wWidth  = (action->bitmap) ? action->bitmap->destsize.cx : 0;
					wHeight = (action->bitmap) ? action->bitmap->destsize.cy : 0;

					wWidth  = max(wWidth,  wCheckWidth)  + (Spacing*2);
					wHeight = max(wHeight, wCheckHeight) + (Spacing*2);

					rcFrame.left  = lpDIS->rcItem.left;
					rcFrame.top   = lpDIS->rcItem.top;
					rcFrame.right = lpDIS->rcItem.bottom - lpDIS->rcItem.top;
					rcFrame.bottom= lpDIS->rcItem.top+wHeight;

					rcBox.left  = rcFrame.left+1;
					rcBox.top   = rcFrame.top+1;
					rcBox.right = rcFrame.right-1;
					rcBox.bottom= rcFrame.bottom-1;

					// Save off context attributes
					nIndexDC = SaveDC(lpDIS->hDC);

					// create brush for selection state
					if (lpDIS->itemState & ODS_SELECTED)
					{
						hFillBrush = GetSysColorBrush(COLOR_HIGHLIGHT);
						SetTextColor(lpDIS->hDC, GetSysColor(COLOR_HIGHLIGHTTEXT));
					}
					else
						hFillBrush = CreateSolidBrush(GetBkColor(lpDIS->hDC));

					SetBkMode(lpDIS->hDC,TRANSPARENT);

					FillRect(lpDIS->hDC, &lpDIS->rcItem, hFillBrush);

					rc = lpDIS->rcItem;
					rc.left += wWidth+Spacing;
					rc.right-= Spacing;

					// Draw out the bitmap associated with the menu item.
					if (action->bitmap)
					{
						pt.x = rcFrame.left+(((rcFrame.right-rcFrame.left) - action->bitmap->destsize.cx)/2);
						pt.y = rcFrame.top +(((rcFrame.bottom-rcFrame.top) - action->bitmap->destsize.cy)/2);

						osDrawBitmap(pt.x, pt.y, action->bitmap, &canvas);

						if (action->enabled)
						{
							// Draw bounding frame
							if (action->checked)
								DrawEdge(lpDIS->hDC,&rcFrame,BDR_SUNKENINNER,BF_RECT);
							else
								if (lpDIS->itemState & ODS_SELECTED)
									DrawEdge(lpDIS->hDC,&rcFrame,BDR_RAISEDINNER,BF_RECT);
						}
					}
					else
					{
						if (action->checked)
							DrawMenuItemCheck(lpDIS->hDC, action, rcFrame, (lpDIS->itemState & ODS_SELECTED));
					}

					rc.left+=Spacing;

					if (!action->enabled)
					{
						// This will give it a disable text look
						if (!(lpDIS->itemState & ODS_SELECTED))
						{
							SetTextColor(lpDIS->hDC, GetSysColor(COLOR_3DHILIGHT));
							OffsetRect(&rc,1,1);
							DrawMenuText(lpDIS->hDC,action,&rc);
							OffsetRect(&rc,-1,-1);
							SetTextColor(lpDIS->hDC,GetSysColor(COLOR_GRAYTEXT));
						}
						else
						{
							COLORREF crGray = (GetSysColor(COLOR_GRAYTEXT) + RGB(64,64,64));
							SetTextColor(lpDIS->hDC,crGray);
						}
					}
					DrawMenuText(lpDIS->hDC,action,&rc);


					RestoreDC(lpDIS->hDC,nIndexDC);
				}
			}
		}
		return TRUE;
	case WM_MENUSELECT:
		if ((HIWORD(wParam) & MF_POPUP) == 0)
		{
			action = getActionHandle(pData->pActionsMap, (UINT) LOWORD(wParam));
			if (action)
			{
				SetWindowText(pData->hStatusBar, action->tooltip ? action->tooltip : "");
			}
		}
		break;
	case WM_ENTERMENULOOP:
		osPushStatusBarContext("");
		break;
	case WM_EXITMENULOOP:
		osPopStatusBarContext();
		break;
	case WM_NOTIFY:
		{
			int i;
			IndicatorHandle indicator;
			LPNMMOUSE lpnm = (LPNMMOUSE) lParam;

			if (lpnm->hdr.code == NM_DBLCLK && lpnm->dwItemSpec > 0)
			{
				i = lpnm->dwItemSpec-1;
				indicator = pData->first_indicator;
				while (i > 0 && indicator != NULL)
				{
					indicator = indicator->next;
					i--;
				}

				if (indicator)
					handleIndicatorCommand(indicator);
			}
		}
		break;
	}

	if (DocumentInterface == 1)
		return DefWindowProcW (hWnd, uMsg, wParam, lParam);
	else
		return DefFrameProcW (hWnd, pData ? pData->hClientWnd : NULL, uMsg, wParam, lParam);
};

LRESULT CALLBACK HMDIFrameFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	LRESULT result = HFrameSharedFunction(2, hWnd, uMsg, wParam, lParam);
	FrameData *pData = (FrameData *) GetWindowLongPtr(hWnd,GWLP_USERDATA);

	switch (uMsg)
	{
	case WM_CREATE:
		{
			HMENU hMenuBar, hMenuWindow;
			CLIENTCREATESTRUCT clientcreate;

			hMenuBar = CreateMenu ();				// Create the menu bar
			SetMenu (hWnd,hMenuBar);				// and associate it with the frame window
			hMenuWindow = CreatePopupMenu();		// Create the "Window" menu
			InsertMenu (hMenuBar,					// add it to the menuBar
						0xFFFFFFFF,					// at the end
						MF_BYPOSITION | MF_POPUP,	// Flags
						(UINT_PTR) hMenuWindow,		// the "Window" menu
						"&Window"					// and set its title
				);
			InsertMenu (hMenuWindow,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+1,"Arrange &Icons");		// Add "Arrange Icons" command
			InsertMenu (hMenuWindow,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+2,"&Tile Vertically");	// Add "Tile Vertically" command
			InsertMenu (hMenuWindow,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+3,"Tile &Horizontally");	// Add "Tile Horizontally" command
			InsertMenu (hMenuWindow,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+4,"&Cascade");			// Add "Cascade" command

			clientcreate.hWindowMenu  = hMenuWindow;
			clientcreate.idFirstChild = OSMenuIDEnd+5;	// Window ids must be generated from OSMenuIDEnd+5

			pData->hClientWnd = CreateWindow ("MDICLIENT",							// The MDICLIENT window class
										NULL,										// The window name
										MDIS_ALLCHILDSTYLES | WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE,
										0,0,										// position (x,y)
										0,0,										// size (w,h)
										hWnd,										// The frame window is the parent
										NULL,										// The menu (none at the moment)
										(HANDLE) ghModule,							// Instance that owns the window
										(LPVOID) &clientcreate						// The CLIENTCREATESTRUCT
										);
			if (!pData->hClientWnd)
				return -1;

			DrawMenuBar(hWnd);
		}
		break;
	case WM_COMMAND:
		if (HIWORD (wParam)==0 && lParam!=0)
		{
		}
		else
			switch (wParam)
			{
				case (OSMenuIDEnd+1):
					SendMessage (pData->hClientWnd,WM_MDIICONARRANGE,0,0);
					break;
				case (OSMenuIDEnd+2):
					SendMessage (pData->hClientWnd,WM_MDITILE,(WPARAM) MDITILE_VERTICAL,0);
					break;
				case (OSMenuIDEnd+3):
					SendMessage (pData->hClientWnd,WM_MDITILE,(WPARAM) MDITILE_HORIZONTAL,0);
					break;
				case (OSMenuIDEnd+4):
					SendMessage (pData->hClientWnd,WM_MDICASCADE,0,0);
					break;
			}
		break;
	}

	return result;
};

LRESULT CALLBACK HSDIFrameFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	LRESULT result = HFrameSharedFunction(1, hWnd, uMsg, wParam, lParam);
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(hWnd,GWLP_USERDATA);

	switch (uMsg)
	{
	case WM_CREATE:
		SetMenu(hWnd, CreateMenu());
		break;
	case WM_ACTIVATE:
		if (pFrameData->hClientWnd)
		{
			if (wParam == WA_INACTIVE)
			{
				if (gInKey)
					handleWindowKeyboard(pFrameData->hClientWnd, evKeyLost, gCurChar, GetModifiers());
				gInKey = FALSE;
				gCurChar = 0;
				handleWindowDeactivate(pFrameData->hClientWnd);
			}
			else
			{
				handleWindowActivate(pFrameData->hClientWnd);
			}
		}
        break;
	}

	return result;
};
