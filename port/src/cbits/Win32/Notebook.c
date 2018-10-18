#include "Notebook.h"
#include "Internals.h"
#include "Handlers_stub.h"

WNDPROC DefTabCtrlProc;

extern LRESULT CALLBACK HCompoundControlFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

static void ResizeNotebookPages(HWND hWnd)
{
	RECT rect;
	int i, nCount;
	TCITEM item;

	GetClientRect(hWnd, &rect);
	SendMessage(hWnd, TCM_ADJUSTRECT, FALSE, (LPARAM) &rect);

	nCount = SendMessage(hWnd, TCM_GETITEMCOUNT, 0, 0);
	for (i = 0; i < nCount; i++)
	{
		item.mask = TCIF_PARAM;
		SendMessage(hWnd, TCM_GETITEM, i, (LPARAM) &item);

		MoveWindow((HWND) item.lParam,rect.left,rect.top,
				(rect.right-rect.left), (rect.bottom-rect.top),
				TRUE);
	}
}

LRESULT CALLBACK HNotebookFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	LRESULT lResult;

	switch (uMsg)
	{
	case WM_SIZE:
		ResizeNotebookPages(hWnd);
		break;
	case WM_NOTIFY:
		{
			NMHDR *pNMHDR = (NMHDR *) lParam;
			int nIndex;
			TCITEM item;
			HWND hNotebookPage;

			if (pNMHDR->hwndFrom == hWnd)
			{
				nIndex = SendMessage(hWnd, TCM_GETCURSEL, 0, 0);
				item.mask = TCIF_PARAM;
				SendMessage(hWnd, TCM_GETITEM, nIndex, (LPARAM) &item);
				hNotebookPage = (HWND) item.lParam;

				switch (pNMHDR->code)
				{
				case TCN_SELCHANGE:
					ShowWindow(hNotebookPage, SW_SHOW);
					handleWindowActivate(hNotebookPage);
					break;
				case TCN_SELCHANGING:
					ShowWindow(hNotebookPage, SW_HIDE);
					handleWindowDeactivate(hNotebookPage);
					break;
				}

				return 0;
			}
		}
    	break;
	}

	lResult = CallWindowProc(DefTabCtrlProc, hWnd, uMsg, wParam, lParam);

	switch (uMsg)
	{
	case WM_CREATE:
		if (lResult >= 0)
		{
			HIMAGELIST hImageList = ImageList_Create(15, 15, ILC_COLOR, 0, 1);
			SendMessage(hWnd, TCM_SETIMAGELIST, 0, (LPARAM) hImageList);
		}
		break;
	}

	return lResult;
}

LRESULT CALLBACK HNotebookPageFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_SETTEXT:
		{
			TCITEM item;
			item.mask    = TCIF_TEXT;
			item.pszText = (char *) lParam;
			SendMessage(GetParent(hWnd), TCM_SETITEM, osGetNotebookPagePos(hWnd), (LPARAM) &item);
		}
		break;
	case WM_DESTROY:
		{
			handleWindowDestroy(hWnd);
			SendMessage(GetParent(hWnd), TCM_DELETEITEM, osGetNotebookPagePos(hWnd), 0);
		}
		break;
	case WM_CTLCOLORSTATIC:
        return DefWindowProcW(hWnd, uMsg, wParam, lParam);
	}

	return HCompoundControlFunction(hWnd, uMsg, wParam, lParam);
}

WindowHandle osCreateNotebook(WindowHandle form)
{
	HWND hNotebook;

	hNotebook = CreateWindowW(
			  L"HNOTEBOOK",
			  NULL,
			  WS_CHILD | WS_TABSTOP,
			  0,0,0,0,
			  form,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hNotebook, WC_TABCONTROL);
};

void osGetNotebookReqSize(WindowHandle notebook, int *res)
{
	RECT rect = {0,0,0,0};
	SendMessage(notebook, TCM_ADJUSTRECT, TRUE, (LPARAM) &rect);

	res[0] = rect.right  - rect.left;
	res[1] = rect.bottom - rect.top;
}

void osSetNotebookLabelsPosition(WindowHandle notebook, PositionType position)
{
	LONG lStyle;

	lStyle = GetWindowLong(notebook, GWL_STYLE);
	lStyle &= ~(TCS_VERTICAL | TCS_RIGHT | TCS_BOTTOM);

	switch (position)
	{
	case PosLeft:   lStyle |= TCS_VERTICAL;             break;
	case PosTop:                                        break;
	case PosRight:  lStyle |= TCS_VERTICAL | TCS_RIGHT; break;
	case PosBottom: lStyle |= TCS_BOTTOM;               break;
	}

	SendMessage(notebook, WM_SETFONT, (WPARAM)NULL, TRUE);
	SetWindowLong(notebook, GWL_STYLE, lStyle);
	SendMessage(notebook, WM_SETFONT, (WPARAM)GetStockObject(DEFAULT_GUI_FONT), MAKELPARAM (TRUE,0));

	ResizeNotebookPages(notebook);
}

PositionType osGetNotebookLabelsPosition(WindowHandle notebook)
{
	LONG lStyle = GetWindowLong(notebook, GWL_STYLE);

	if (lStyle & TCS_VERTICAL)
		if (lStyle & TCS_RIGHT)
			return PosRight;
		else
			return PosLeft;
	else
		if (lStyle & TCS_BOTTOM)
			return PosBottom;
		else
			return PosTop;
}

int osGetNotebookSelection(WindowHandle notebook)
{
	return SendMessage(notebook, TCM_GETCURSEL, 0, 0);
};

void osSetNotebookSelection(WindowHandle notebook, int index)
{
	TCITEM item;
	int nOldIndex;

	item.mask = TCIF_PARAM;

	nOldIndex = SendMessage(notebook, TCM_GETCURSEL, 0, 0);
	SendMessage(notebook, TCM_SETCURSEL, index, 0);

	SendMessage(notebook, TCM_GETITEM, nOldIndex, (LPARAM) &item);
	ShowWindow((HWND) item.lParam, SW_HIDE);

	SendMessage(notebook, TCM_GETITEM, index, (LPARAM) &item);
	ShowWindow((HWND) item.lParam, SW_SHOW);
};

WindowHandle osInsertNotebookPage(WindowHandle notebook, int pos)
{
	HWND hWnd;
	RECT rect;
	TCITEM item;
	int nCount;

	nCount = SendMessage(notebook, TCM_GETITEMCOUNT, 0, 0);
	if (pos < 0) pos = nCount;

	GetWindowRect(notebook, &rect);
	SendMessage(notebook, TCM_ADJUSTRECT, FALSE, (LPARAM) &rect);

	hWnd = CreateWindowW(L"HNOTEBOOKPAGE",
						 NULL,
						 WS_CHILD | ((nCount > 0) ? 0 : WS_VISIBLE),
						 0,0,rect.right-rect.left,rect.bottom-rect.top,
						 notebook,
						 NULL,
						 ghModule,
						 NULL
						);

	item.mask    = TCIF_PARAM | TCIF_TEXT;
	item.lParam  = (LPARAM) hWnd;
	item.pszText = "";
	SendMessageW(notebook, TCM_INSERTITEM, pos, (LPARAM) &item);
	return hWnd;
}

PortString osGetNotebookPageTitle(WindowHandle window)
{
    int nLen = GetWindowTextLengthW(window);
    PortString buffer = (PortString) malloc((nLen+1)*sizeof(wchar_t));
    if (buffer) GetWindowTextW(window, buffer, nLen+1);
    return buffer;
};

void osSetNotebookPageTitle(WindowHandle window, PortString txt)
{
    SetWindowTextW(window, txt);
};

int osGetNotebookPagePos(WindowHandle window)
{
	HWND hNotebook = GetParent(window);
	int i, nCount = SendMessage(hNotebook, TCM_GETITEMCOUNT, 0, 0);
	TCITEM item;

	for (i = 0; i < nCount; i++)
	{
		item.mask = TCIF_PARAM;
		SendMessage(hNotebook, TCM_GETITEM, i, (LPARAM) &item);

		if (((HWND) item.lParam) == window)
			return i;
	}

	return -1;
}

void osDestroyNotebookPage(WindowHandle window)
{
	DestroyWindow(window);
}

void osGetNotebookPageSize(WindowHandle window, int *res)
{
    RECT rect;
    GetWindowRect(window,&rect);
    res[0] = rect.right-rect.left;
    res[1] = rect.bottom-rect.top;
}

void osSetNotebookPageBitmap(WindowHandle window, BitmapHandle bitmap)
{
	TCITEM item;
	int i, nPos, nImage, nCount;
	HWND hNotebook = GetParent(window);

	// find the page position and an index of the old bitmap
	nPos   = -1;
	nImage = -1;
	nCount = SendMessage(hNotebook, TCM_GETITEMCOUNT, 0, 0);
	for (i = 0; i < nCount; i++)
	{
		item.mask = TCIF_PARAM | TCIF_IMAGE;
		SendMessage(hNotebook, TCM_GETITEM, i, (LPARAM) &item);

		if (((HWND) item.lParam) == window)
		{
			nPos   = i;
			nImage = item.iImage;
			break;
		}
	}

	// return if the page is not found
	if (nPos < 0)
		return;

	if (nImage > 0)
	{
		// remove old bitmap

		BOOL bUsed = FALSE;
		for (i = 0; i < nCount; i++)
		{
			item.mask = TCIF_PARAM | TCIF_IMAGE;
			SendMessageW(hNotebook, TCM_GETITEM, i, (LPARAM) &item);

			if (((HWND) item.lParam) != window && item.iImage == nImage)
			{
				bUsed = TRUE;
				break;
			}
		}

		if (!bUsed)
			SendMessage(hNotebook, TCM_REMOVEIMAGE, nImage, 0);
	}

	// set new bitmap
	if (bitmap)
	{
		HIMAGELIST hImageList = (HIMAGELIST) SendMessage(hNotebook, TCM_GETIMAGELIST, 0, 0);
		item.iImage = ImageList_Add(hImageList, bitmap->hBitmap, bitmap->hBitmap);
	}
	else
	{
		item.iImage = -1;
	}

	SendMessageW(hNotebook, TCM_SETITEM, nPos, (LPARAM) &item);
}

int osGetNotebookPageCount(WindowHandle notebook)
{
	return SendMessageW(notebook, TCM_GETITEMCOUNT, 0, 0);
}
