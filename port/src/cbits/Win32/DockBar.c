#include "Types.h"
#include "DockBar.h"
#include "Action.h"
#include "Internals.h"
#include "Handlers_stub.h"


typedef struct
{
	int nSize;
	int nOffset;
	HWND hWnd;
} BarData;

typedef struct
{
	int nSize;
	int nOffset;
	int nBarCount;
	BarData *pBars;
} BandData;

typedef struct
{
	int nBandCount;
	BandData *pBands;
} DockBarData;

static void RecalcDockBarLayout(DockBarData *pData, BOOL bVert)
{
	int i, j;
	BarData *pBar;
	BandData *pBand;
	int nBarOffset, nBandOffset;
	SIZE sz;

	nBandOffset = 0;
	for (i = 0; i < pData->nBandCount; i++)
	{
		pBand = &pData->pBands[i];

		pBand->nSize = 0;
		pBand->nOffset = nBandOffset;

		nBarOffset = 0;
		for (j = 0; j < pBand->nBarCount; j++)
		{
			pBar = &pBand->pBars[j];
			GetToolBarSize(pBar->hWnd, &sz);

			pBar->nOffset = max(pBar->nOffset, nBarOffset);
			if (bVert)
			{
				pBand->nSize = max(pBand->nSize, sz.cx);
				pBar->nSize = sz.cy;

				MoveWindow(pBar->hWnd,pBand->nOffset,pBar->nOffset,sz.cx,sz.cy,TRUE);
			}
			else
			{
				pBand->nSize = max(pBand->nSize, sz.cy);
				pBar->nSize = sz.cx;

				MoveWindow(pBar->hWnd,pBar->nOffset,pBand->nOffset,sz.cx,sz.cy,TRUE);
			}

			nBarOffset = pBar->nOffset + pBar->nSize;
		}

		nBandOffset = pBand->nOffset + pBand->nSize;
	}
}

static int GetDockBarSize(HWND hDockBar)
{
    int i, nSize;
    DockBarData *pData;

	nSize = 0;
	pData = (DockBarData *) GetWindowLongPtrW(hDockBar, GWLP_USERDATA);

	RecalcDockBarLayout(pData, GetWindowLongPtrW(hDockBar, GWL_STYLE) & CCS_VERT);

	for (i = 0; i < pData->nBandCount; i++)
		nSize += pData->pBands[i].nSize;

	return nSize;
}

void RelayoutFrameBars()
{
	RECT rect;
	FrameData *pData;
	int nWidth, nHeight;
	int nLeft, nTop, nRight, nBottom;
	int nStatusHeight;

	if (!ghWndFrame)
		return;

	pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	if (IsWindowVisible(pData->hStatusBar))
		nStatusHeight = 20;
	else
		nStatusHeight = 0;

	GetClientRect(ghWndFrame, &rect);

	nWidth  = rect.right-rect.left;
	nHeight = rect.bottom-rect.top-nStatusHeight;
	nLeft   = GetDockBarSize(pData->hLeftBar);
	nTop    = GetDockBarSize(pData->hTopBar);
	nRight  = GetDockBarSize(pData->hRightBar);
	nBottom = GetDockBarSize(pData->hBottomBar);

	MoveWindow(pData->hLeftBar,  0,nTop,nLeft,nHeight-(nBottom+nTop),TRUE);
	MoveWindow(pData->hTopBar,   0,0,nWidth,nTop,TRUE);
	MoveWindow(pData->hRightBar, nWidth-nRight,nTop,nRight,nHeight-(nBottom+nTop),TRUE);
	MoveWindow(pData->hBottomBar,0,nHeight-nBottom,nWidth,nBottom,TRUE);

	MoveWindow(pData->hClientWnd,nLeft,nTop,nWidth-(nRight+nLeft),nHeight-(nBottom+nTop),TRUE);

	MoveWindow(pData->hStatusBar,0,nHeight,nWidth,nStatusHeight,TRUE);
}

LRESULT CALLBACK HDockBarFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	DockBarData *pData = (DockBarData *) GetWindowLongPtrW(hWnd, GWLP_USERDATA);
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	switch (uMsg)
	{
	case WM_CREATE:
		{
			HMENU hMenu;

			if (((CREATESTRUCT *) lParam)->style & WS_CHILD)
			{
				pData = malloc(sizeof(DockBarData));
				if (!pData) return -1;

				pData->nBandCount = 0;
				pData->pBands     = NULL;

				SetWindowLongPtrW(hWnd, GWLP_USERDATA, (LONG_PTR) pData);
			}

			hMenu = GetSystemMenu(hWnd, FALSE);
			if (hMenu)
			{
				DeleteMenu(hMenu, SC_RESTORE,  MF_BYCOMMAND);
				DeleteMenu(hMenu, SC_MINIMIZE, MF_BYCOMMAND);
				DeleteMenu(hMenu, SC_MAXIMIZE, MF_BYCOMMAND);
				DeleteMenu(hMenu, SC_CLOSE,    MF_BYCOMMAND);
				AppendMenu(hMenu, MF_STRING, SC_CLOSE, "Hide");
			}
		}
		break;
	case WM_DESTROY:
		if (pData)
		{
			int i;

			SetWindowLongPtrW(hWnd, GWLP_USERDATA, 0);
			for (i = 0; i < pData->nBandCount; i++)
				free(pData->pBands[i].pBars);
			free(pData->pBands);
			free(pData);
		}
		break;
	case WM_CLOSE:
		ShowWindow(hWnd, SW_HIDE);
		return 0;
	case WM_COMMAND:
		osActivateAction(LOWORD(wParam));
		break;
	case WM_NOTIFY:
		{
			LPNMHDR lpNMHdr = (LPNMHDR) lParam;

			if (lpNMHdr->code == TBN_DROPDOWN)
			{
				LPNMTOOLBAR lpNMToolBar = (LPNMTOOLBAR) lpNMHdr;

				ActionHandle action = getActionHandle(pFrameData->pActionsMap, lpNMToolBar->iItem);
				if (action && action->menu)
				{
					RECT rect;
					int nIndex;

					memset(&rect, 0, sizeof(rect));
					nIndex = SendMessage(lpNMHdr->hwndFrom, TB_COMMANDTOINDEX, action->id, 0);
					SendMessage(lpNMHdr->hwndFrom, TB_GETITEMRECT, nIndex, (LPARAM)&rect);
					MapWindowPoints(lpNMHdr->hwndFrom, NULL, (LPPOINT) &rect, 2);

					TrackPopupMenu(action->menu->hMenu, TPM_LEFTALIGN | TPM_TOPALIGN, rect.left, rect.bottom, 0, ghWndFrame, NULL);
				}
			}
			else
				if (lpNMHdr->code == TTN_NEEDTEXT)
				{
					LPNMTTDISPINFO lpNMTTDispInfo = (LPNMTTDISPINFO) lpNMHdr;

					ActionHandle action = getActionHandle(pFrameData->pActionsMap, lpNMHdr->idFrom);
					if (action)
						lpNMTTDispInfo->lpszText = action->tooltip;
				}
		}
		break;
	case WM_SIZE:
		if (!pData)
		{
			int nWidth  = LOWORD(lParam);
            int nHeight = HIWORD(lParam);

        	MoveWindow(GetWindow(hWnd, GW_CHILD),0,0,nWidth,nHeight,TRUE);
		}
		break;
	case WM_NCLBUTTONDOWN:
		if (wParam == HTCAPTION)
			SendMessage(GetWindow(hWnd, GW_CHILD), uMsg, wParam, lParam);
		break;
	}

	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

void DockToolBarToRect(HWND hDockBar, HWND hWnd, RECT rect)
{
	int nBandNum;
	int nPosition;
	int nOffset;
	int nCentralOffset;
	DockBarData *pData = (DockBarData *) GetWindowLongPtrW(hDockBar, GWLP_USERDATA);
	BOOL bVert = GetWindowLongPtrW(hDockBar, GWL_STYLE) & CCS_VERT;
	BandData *pBand;

	RecalcDockBarLayout(pData, bVert);

	if (bVert)
	{
		nOffset = rect.top;
		nCentralOffset = (rect.right-rect.left)/2;
	}
	else
	{
		nOffset = rect.left;
		nCentralOffset = (rect.bottom-rect.top)/2;
	}

	for (nBandNum = 0; nBandNum < pData->nBandCount; nBandNum++)
	{
		if (pData->pBands[nBandNum].nOffset <= nCentralOffset &&
		    nCentralOffset <= pData->pBands[nBandNum].nOffset+pData->pBands[nBandNum].nSize)
			break;
	}

	if (nBandNum >= pData->nBandCount)
		nPosition = 0;
	else
	{
		pBand = &pData->pBands[nBandNum];

		if (nOffset < pBand->pBars[0].nOffset)
			nPosition = 0;
		else
		{
			for (nPosition = 1; nPosition < pBand->nBarCount; nPosition++)
			{
				if (nOffset < pBand->pBars[nPosition].nOffset)
					break;
			}
		}
	}

	DockToolBar(hDockBar, hWnd, nBandNum, nPosition, nOffset);
}

void DockToolBar(HWND hDockBar, HWND hWnd, int nBandNum, int nPosition, int nOffset)
{
	BarData *pBar;
	BandData *pBand;
	DockBarData *pData = (DockBarData *) GetWindowLongPtrW(hDockBar, GWLP_USERDATA);

	SetParent(hWnd, hDockBar);

	if (pData)
	{
		if (nBandNum >= pData->nBandCount)
		{
			pData->pBands = realloc(pData->pBands, (pData->nBandCount+1)*sizeof(BandData));
			memset(pData->pBands+pData->nBandCount, 0, sizeof(BandData));
			nBandNum = pData->nBandCount;
			pData->nBandCount++;
		}
		pBand = &pData->pBands[nBandNum];

		if (nPosition >= pBand->nBarCount)
			nPosition = pBand->nBarCount;
		pBand->pBars = realloc(pBand->pBars, (pBand->nBarCount+1)*sizeof(BarData));
		memmove(pBand->pBars+nPosition+1, pBand->pBars+nPosition, (pBand->nBarCount-nPosition)*sizeof(BarData));
		pBand->nBarCount++;

		pBar = &pBand->pBars[nPosition];
		memset(pBar, 0, sizeof(BarData));
		pBar->nOffset = nOffset;
		pBar->hWnd = hWnd;

		RecalcDockBarLayout(pData, GetWindowLongPtrW(hWnd, GWL_STYLE) & CCS_VERT);
	}
}

void UndockToolBar(HWND hDockBar, HWND hWnd)
{
	int i, j;
	BarData *pBar;
	BandData *pBand;
	DockBarData *pData = (DockBarData *) GetWindowLongPtrW(hDockBar, GWLP_USERDATA);

	SetParent(hWnd, NULL);

	if (pData)
	{
		for (i = 0; i < pData->nBandCount; i++)
		{
			pBand = &pData->pBands[i];

			for (j = 0; j < pBand->nBarCount; j++)
			{
				pBar = &pBand->pBars[j];

				if (pBar->hWnd == hWnd)
				{
					memmove(pBand->pBars+j, pBand->pBars+j+1, (pBand->nBarCount-j-1)*sizeof(BarData));
					pBand->pBars = realloc(pBand->pBars, (pBand->nBarCount-1)*sizeof(BarData));
					pBand->nBarCount--;

					if (pBand->nBarCount == 0)
					{
						memmove(pData->pBands+i, pData->pBands+i+1, (pData->nBandCount-i-1)*sizeof(BandData));
						pData->pBands = realloc(pData->pBands, (pData->nBandCount-1)*sizeof(BandData));
						pData->nBandCount--;
					}

					return;
				}
			}
		}
	}
	else
	{
		DestroyWindow(hDockBar);
	}
}
