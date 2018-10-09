#include "ListBox.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include <windowsx.h>

static unsigned char bits[] =
{0x42,0x4D,0x52,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x76,0x00,0x00,0x00,0x28,0x00,
 0x00,0x00,0x21,0x00,0x00,0x00,0x0B,0x00,0x00,0x00,0x01,0x00,0x04,0x00,0x00,0x00,
 0x00,0x00,0xDC,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x10,0x00,
 0x00,0x00,0x10,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0x00,0x00,0x80,
 0x00,0x00,0x00,0x80,0x80,0x00,0x80,0x00,0x00,0x00,0x80,0x00,0x80,0x00,0x80,0x80,
 0x00,0x00,0xC0,0xC0,0xC0,0x00,0x80,0x80,0x80,0x00,0x00,0x00,0xFF,0x00,0x00,0xFF,
 0x00,0x00,0x00,0xFF,0xFF,0x00,0xFF,0x00,0x00,0x00,0xFF,0x00,0xFF,0x00,0xFF,0xFF,
 0x00,0x00,0xFF,0xFF,0xFF,0x00,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,
 0x88,0x88,0x88,0x88,0x88,0x88,0x80,0x00,0x00,0x00,0x8F,0xFF,0xFF,0xFF,0xFF,0x88,
 0xFF,0xFF,0xFF,0xFF,0xF8,0x87,0x77,0x77,0x77,0x77,0x80,0x00,0x00,0x00,0x8F,0xFF,
 0xFF,0xFF,0xFF,0x88,0xFF,0xF0,0xFF,0xFF,0xF8,0x87,0x77,0x87,0x77,0x77,0x80,0x00,
 0x00,0x00,0x8F,0xFF,0xFF,0xFF,0xFF,0x88,0xFF,0x00,0x0F,0xFF,0xF8,0x87,0x78,0x88,
 0x77,0x77,0x80,0x00,0x00,0x00,0x8F,0xFF,0xFF,0xFF,0xFF,0x88,0xF0,0x00,0x00,0xFF,
 0xF8,0x87,0x88,0x88,0x87,0x77,0x80,0x00,0x00,0x00,0x8F,0xFF,0xFF,0xFF,0xFF,0x88,
 0xF0,0x0F,0x00,0x0F,0xF8,0x87,0x88,0x78,0x88,0x77,0x80,0x00,0x00,0x00,0x8F,0xFF,
 0xFF,0xFF,0xFF,0x88,0xF0,0xFF,0xF0,0x00,0xF8,0x87,0x87,0x77,0x88,0x87,0x80,0x00,
 0x00,0x00,0x8F,0xFF,0xFF,0xFF,0xFF,0x88,0xFF,0xFF,0xFF,0x00,0xF8,0x87,0x77,0x77,
 0x78,0x87,0x80,0x00,0x00,0x00,0x8F,0xFF,0xFF,0xFF,0xFF,0x88,0xFF,0xFF,0xFF,0xF0,
 0xF8,0x87,0x77,0x77,0x77,0x87,0x80,0x00,0x00,0x00,0x8F,0xFF,0xFF,0xFF,0xFF,0x88,
 0xFF,0xFF,0xFF,0xFF,0xF8,0x87,0x77,0x77,0x77,0x77,0x80,0x00,0x00,0x00,0x88,0x88,
 0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x80,0x00,
 0x00,0x00
};

static HBITMAP GetCheckListBitmap(HDC hDC)
{
	static HBITMAP hCheckListBitmap = NULL;

	if (!hCheckListBitmap)
	{
		BITMAPINFO *lpbmi = (BITMAPINFO *) (bits+sizeof(BITMAPFILEHEADER));
		VOID *lpbInit = bits+((BITMAPFILEHEADER *) bits)->bfOffBits;

		hCheckListBitmap = CreateDIBitmap(hDC,&lpbmi->bmiHeader,CBM_INIT,lpbInit,lpbmi,DIB_RGB_COLORS);
	}

	return hCheckListBitmap;
}

void DrawCheckListBoxItem(LPDRAWITEMSTRUCT lpDIS)
{
	if ((((LONG) lpDIS->itemID) >= 0) && (lpDIS->itemAction & (ODA_DRAWENTIRE | ODA_SELECT)))
	{
		int cyItem;
		BOOL fDisabled;
		HFONT hFont, hOldFont;
		COLORREF newBkColor, oldBkColor;
		COLORREF newTextColor, oldTextColor;
		TEXTMETRIC tm;
		int nTextLen;
		LPCSTR lpszText;
		HDC hMemDC;
		int nItemData;

		cyItem = SendMessage(lpDIS->hwndItem, LB_GETITEMHEIGHT, lpDIS->itemID, 0);
		nItemData = SendMessage(lpDIS->hwndItem, LB_GETITEMDATA, lpDIS->itemID, 0);

		fDisabled = !IsWindowEnabled(lpDIS->hwndItem);

		newTextColor = fDisabled ? RGB(0x80, 0x80, 0x80) : GetSysColor(COLOR_WINDOWTEXT);  // light gray
		oldTextColor = SetTextColor(lpDIS->hDC, newTextColor);

		newBkColor = GetSysColor(COLOR_WINDOW);
		oldBkColor = SetBkColor(lpDIS->hDC, newBkColor);

		if (newTextColor == newBkColor)
			newTextColor = RGB(0xC0, 0xC0, 0xC0);   // dark gray

		if (!fDisabled && ((lpDIS->itemState & ODS_SELECTED) != 0))
		{
			SetTextColor(lpDIS->hDC, GetSysColor(COLOR_HIGHLIGHTTEXT));
			SetBkColor(lpDIS->hDC, GetSysColor(COLOR_HIGHLIGHT));
		}

		hFont = (HFONT) SendMessage(lpDIS->hwndItem,WM_GETFONT,0,0);
		hOldFont = SelectObject(lpDIS->hDC, hFont);
		GetTextMetrics(lpDIS->hDC, &tm);

		nTextLen = SendMessage(lpDIS->hwndItem, LB_GETTEXTLEN, lpDIS->itemID, 0);
		lpszText = rmalloc(nTextLen+1);
		SendMessage(lpDIS->hwndItem, LB_GETTEXT, lpDIS->itemID, (LPARAM) lpszText);

		ExtTextOut(lpDIS->hDC, lpDIS->rcItem.left + cyItem,
			lpDIS->rcItem.top + max(0, (cyItem - tm.tmHeight) / 2),
			ETO_OPAQUE, &lpDIS->rcItem, lpszText, nTextLen, NULL);

		hMemDC = CreateCompatibleDC(lpDIS->hDC);
		SelectObject(hMemDC, GetCheckListBitmap(lpDIS->hDC));
		BitBlt(lpDIS->hDC, 1, lpDIS->rcItem.top+(cyItem-11)/2, 11, 11,
		       hMemDC, (nItemData <= 0) ? 0 : 11, 0,
		       SRCCOPY);
		DeleteDC(hMemDC);

		rfree((void*) lpszText);

		SelectObject(lpDIS->hDC, hOldFont);
		SetBkColor(lpDIS->hDC, oldBkColor);
		SetTextColor(lpDIS->hDC, oldTextColor);
	}

	if ((lpDIS->itemAction & ODA_FOCUS) != 0)
		DrawFocusRect(lpDIS->hDC, &lpDIS->rcItem);
}

WNDPROC DefCheckListBoxProc = NULL;

LRESULT CALLBACK HCheckListBoxFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_LBUTTONDOWN:
		{
			int cyItem;
			int nTopIndex, nIndex, nCount;
			BOOL bCheck;
			POINT pos;
			RECT rect;

            pos.x = GET_X_LPARAM(lParam);
            pos.y = GET_Y_LPARAM(lParam);

			SetFocus(hWnd);

			cyItem = SendMessage(hWnd, LB_GETITEMHEIGHT, 0, 0);
			nCount = SendMessage(hWnd, LB_GETCOUNT, 0, 0);
			nTopIndex = SendMessage(hWnd, LB_GETTOPINDEX, 0, 0);

			if (pos.y < cyItem * nCount)
			{
				nIndex = nTopIndex + pos.y / cyItem;
				if (pos.x < cyItem)
				{
					bCheck = SendMessage(hWnd, LB_GETITEMDATA, nIndex, 0);
					SendMessage(hWnd, LB_SETITEMDATA, nIndex, !bCheck);

					handleControlCommand(hWnd);

					SendMessage(hWnd, LB_GETITEMRECT, nIndex, (LPARAM)&rect);
					rect.right = rect.left + cyItem;
					InvalidateRect(hWnd, &rect, TRUE);
				}
			}
		}
   		break;
	};

	return CallWindowProc(DefCheckListBoxProc, hWnd, uMsg, wParam, lParam);
}

WindowHandle osCreateListBox(WindowHandle window, BOOL multisel)
{
	HWND hListBox;

	hListBox = CreateWindowExW(
			  WS_EX_CLIENTEDGE,
			  L"LISTBOX",
			  NULL,
			  WS_CHILD | WS_VSCROLL | WS_BORDER | WS_TABSTOP | LBS_NOTIFY | LBS_NOINTEGRALHEIGHT | (multisel ? LBS_EXTENDEDSEL : 0),
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hListBox, "LISTBOX");
};

WindowHandle osCreateCheckListBox(WindowHandle window)
{
	HWND hListBox;

	hListBox = CreateWindowExW(
			  WS_EX_CLIENTEDGE,
			  L"HCHECKLISTBOX",
			  NULL,
			  WS_CHILD | WS_VSCROLL | WS_BORDER | LBS_NOTIFY | WS_TABSTOP | LBS_HASSTRINGS | LBS_OWNERDRAWFIXED,
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hListBox, "LISTBOX");
};

void osAppendListBoxItem(WindowHandle listbox, PortString title)
{
	SendMessageW(listbox, LB_ADDSTRING, 0, (LPARAM) title);
	osForceContainerReLayout(listbox);
};

void osInsertListBoxItem(WindowHandle listbox, int index, PortString title)
{
	SendMessageW(listbox, LB_INSERTSTRING, index, (LPARAM) title);
	osForceContainerReLayout(listbox);
};

void osRemoveListBoxItem(WindowHandle listbox, int index)
{
	SendMessage(listbox, LB_DELETESTRING, index, 0);
	osForceContainerReLayout(listbox);
};

void osRemoveAllListBoxItems(WindowHandle listbox)
{
	SendMessage(listbox, LB_RESETCONTENT,0,0);
	osForceContainerReLayout(listbox);
};

void osGetListBoxReqSize(WindowHandle listbox, int *res)
{
	SIZE sz;
	int i,nCount, nLen, width, height;
	char *buffer;
	HDC hDC = GetDC(listbox);
	HFONT hFont = (HFONT) SendMessage(listbox,WM_GETFONT,0,0);

	if (hFont)
		SelectObject(hDC, hFont);

	width  = 10;
	height = 2;
	nCount = SendMessage(listbox,LB_GETCOUNT,0,0);
	for (i = 0; i < nCount; i++)
	{
		nLen = SendMessage(listbox,LB_GETTEXTLEN,i,0);
		buffer = (char *) rmalloc(nLen+1);
		nLen = SendMessage(listbox,LB_GETTEXT,i,(LPARAM)buffer);

		GetTextExtentPoint32(hDC, buffer, nLen, &sz);

		rfree(buffer);

		if (width < sz.cx) width = sz.cx;
		height += sz.cy;
	}

	ReleaseDC(listbox, hDC);

	res[0] = width + GetSystemMetrics(SM_CXBORDER)*2 + GetSystemMetrics(SM_CXVSCROLL);
	res[1] = height+ GetSystemMetrics(SM_CYBORDER)*2;
};

int osGetListBoxSingleSelection(WindowHandle listbox)
{
	return SendMessage(listbox, LB_GETCURSEL, 0, 0);
};

BOOL osGetListBoxItemSelectState(WindowHandle listbox, int index)
{
	wchar_t buffer[20];
    GetClassNameW(listbox,buffer,sizeof(buffer));

    if (_wcsicmp(buffer, L"HCHECKLISTBOX") == 0)
		return SendMessageW(listbox, LB_GETITEMDATA, index, 0) > 0;
	else
		return SendMessageW(listbox, LB_GETSEL, index, 0) > 0;
}

void osSetListBoxSingleSelection(WindowHandle listbox, int index)
{
	SendMessageW(listbox, LB_SETCURSEL, index, 0);
	handleControlCommand(listbox);
};

void osSetListBoxItemSelectState(WindowHandle listbox, int index, BOOL state)
{
	wchar_t buffer[20];
    GetClassNameW(listbox,buffer,sizeof(buffer));

    if (_wcsicmp(buffer, L"HCHECKLISTBOX") == 0)
	{
		RECT rect;
		int cyItem = SendMessageW(listbox, LB_GETITEMHEIGHT, 0, 0);

		SendMessageW(listbox, LB_SETITEMDATA, index, state);

		SendMessageW(listbox, LB_GETITEMRECT, index, (LPARAM)&rect);
		rect.right = rect.left + cyItem;
		InvalidateRect(listbox, &rect, TRUE);
	}
	else
		SendMessageW(listbox, LB_SETSEL, state, index);
};

int osGetListBoxCurrentItem(WindowHandle listbox)
{
	return SendMessage(listbox, LB_GETCARETINDEX, 0, 0);
}
