#include "Splitter.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include <windowsx.h>

typedef struct
{
	int nSplitterPos;
	BOOL bInDragMode;
	HWND hCtrl1, hCtrl2;
} SplitterData;

#define SPLIT_LINE_SIZE 6

static void CalcPaneRects(HWND hWnd, SplitterData *pData, RECT *rects)
{
	GetClientRect(hWnd, &rects[0]);

	rects[1] = rects[0];
	rects[2] = rects[0];

	if (GetWindowLong(hWnd, GWL_STYLE) & CCS_VERT)
	{
		rects[0].top    = pData->nSplitterPos-(SPLIT_LINE_SIZE-2)/2;
		rects[0].bottom = pData->nSplitterPos+(SPLIT_LINE_SIZE-2)/2;
		rects[1].bottom = pData->nSplitterPos-SPLIT_LINE_SIZE/2;
		rects[2].top    = pData->nSplitterPos+SPLIT_LINE_SIZE/2;
	}
	else
	{
		rects[0].left  = pData->nSplitterPos-(SPLIT_LINE_SIZE-2)/2;
		rects[0].right = pData->nSplitterPos+(SPLIT_LINE_SIZE-2)/2;
		rects[1].right = pData->nSplitterPos-SPLIT_LINE_SIZE/2;
		rects[2].left  = pData->nSplitterPos+SPLIT_LINE_SIZE/2;
	}
}

static void RelayoutPanes(HWND hWnd, SplitterData *pData)
{
	RECT rects[3];
	CalcPaneRects(hWnd, pData, rects);

	if (pData->hCtrl1)
		MoveWindow(pData->hCtrl1, rects[1].left, rects[1].top, rects[1].right-rects[1].left, rects[1].bottom-rects[1].top, TRUE);

	if (pData->hCtrl2)
		MoveWindow(pData->hCtrl2, rects[2].left, rects[2].top, rects[2].right-rects[2].left, rects[2].bottom-rects[2].top, TRUE);

	InvalidateRect(hWnd, NULL, TRUE);
}

static void UpdateSplitterPos(HWND hWnd, SplitterData *pData, int pos)
{
	RECT rect;
	int nMaxPos;

	GetClientRect(hWnd, &rect);
	if (GetWindowLong(hWnd, GWL_STYLE) & CCS_VERT)
		nMaxPos = rect.bottom-rect.top;
	else
		nMaxPos = rect.right-rect.left;

	if (pos > SPLIT_LINE_SIZE/2 && pos < nMaxPos-SPLIT_LINE_SIZE/2)
	{
		pData->nSplitterPos = pos;
		RelayoutPanes(hWnd, pData);
	}
}

LRESULT CALLBACK HSplitterFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	POINT pos;
	RECT rects[3];
	SplitterData *pData = (SplitterData *) GetWindowLongPtr(hWnd, GWLP_USERDATA);

	switch (uMsg)
	{
	case WM_CREATE:
		{
			pData = malloc(sizeof(SplitterData));
			pData->nSplitterPos = SPLIT_LINE_SIZE/2;
			pData->bInDragMode  = FALSE;

			pData->hCtrl1 = CreateWindow(
				"HCOMPOUND",
				NULL,
				WS_CHILD | WS_VISIBLE | WS_BORDER | WS_HSCROLL | WS_VSCROLL,
				0,0,0,0,
				hWnd,
				NULL,
				ghModule,
				NULL
				);
			pData->hCtrl2 = CreateWindow(
				"HCOMPOUND",
				NULL,
				WS_CHILD | WS_VISIBLE | WS_BORDER | WS_HSCROLL | WS_VSCROLL,
				0,0,0,0,
				hWnd,
				NULL,
				ghModule,
				NULL
				);

			SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG_PTR) pData);
			RelayoutPanes(hWnd, pData);
		}
		break;
	case WM_DESTROY:
		free(pData);
		break;
	case WM_PAINT:
		{
			PAINTSTRUCT ps;
			HDC hDC = BeginPaint(hWnd, &ps);

			CalcPaneRects(hWnd, pData, rects);

			FillRect(hDC, &rects[0], GetSysColorBrush(COLOR_BTNFACE));

			if (GetWindowLong(hWnd, GWL_STYLE) & CCS_VERT)
			{
				MoveToEx(hDC, rects[0].left, rects[0].top-1, NULL);
				LineTo(hDC, rects[0].right, rects[0].top-1);

				MoveToEx(hDC, rects[0].left, rects[0].bottom+1, NULL);
				LineTo(hDC, rects[0].right, rects[0].bottom+1);
			}
			else
			{
				MoveToEx(hDC, rects[0].left-1, rects[0].top, NULL);
				LineTo(hDC, rects[0].left-1, rects[0].bottom);

				MoveToEx(hDC, rects[0].right+1, rects[0].top, NULL);
				LineTo(hDC, rects[0].right+1, rects[0].bottom);
			}

			if (!pData->hCtrl1)
				FillRect(hDC, &rects[1], GetSysColorBrush(COLOR_BTNFACE));

			if (!pData->hCtrl2)
				FillRect(hDC, &rects[2], GetSysColorBrush(COLOR_BTNFACE));

			EndPaint(hWnd, &ps);
		}
		break;
	case WM_ERASEBKGND:
		return (LRESULT) NULL;
	case WM_SIZE:
		RelayoutPanes(hWnd, pData);
		break;
	case WM_LBUTTONDOWN:
		pos.x = GET_X_LPARAM(lParam);
		pos.y = GET_Y_LPARAM(lParam);
		CalcPaneRects(hWnd, pData, rects);

		if (PtInRect(&rects[0],pos))
		{
			pData->bInDragMode = TRUE;
			SetCapture(hWnd);

			if (GetWindowLong(hWnd, GWL_STYLE) & CCS_VERT)
				SetCursor(LoadCursor(NULL, IDC_SIZENS));
			else
				SetCursor(LoadCursor(NULL, IDC_SIZEWE));
		}
		break;
	case WM_LBUTTONUP:
		ReleaseCapture();
		break;
	case WM_MOUSEMOVE:
		pos.x = GET_X_LPARAM(lParam);
		pos.y = GET_Y_LPARAM(lParam);
		CalcPaneRects(hWnd, pData, rects);

		if (pData->bInDragMode)
		{
			if (GetWindowLong(hWnd, GWL_STYLE) & CCS_VERT)
				UpdateSplitterPos(hWnd, pData, pos.y);
			else
				UpdateSplitterPos(hWnd, pData, pos.x);
		}

		if (PtInRect(&rects[0],pos))
		{
			if (GetWindowLong(hWnd, GWL_STYLE) & CCS_VERT)
				SetCursor(LoadCursor(NULL, IDC_SIZENS));
			else
				SetCursor(LoadCursor(NULL, IDC_SIZEWE));
		}
		else
			SetCursor(LoadCursor(NULL, IDC_ARROW));
		break;
	case WM_CAPTURECHANGED:
		pData->bInDragMode = FALSE;
		break;
	}

	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

WindowHandle osCreateSplitter(WindowHandle window, BOOL isVert, /* out */ WindowHandle *panes)
{
	HWND hSplitter;
	SplitterData *pData;

	hSplitter = CreateWindow(
			  "HSPLITTER",
			  NULL,
			  WS_CHILD | WS_TABSTOP | (isVert ? CCS_VERT : 0),
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);

	pData = (SplitterData *) GetWindowLongPtr(hSplitter, GWLP_USERDATA);
	panes[0] = pData->hCtrl1;
	panes[1] = pData->hCtrl2;

	return checkWindow(hSplitter, "HSPLITTER");
}

void osGetSplitterReqSize(WindowHandle splitter, int *res)
{
	res[0] = 100;
	res[1] = 100;
}

void osGetSplitterRange(WindowHandle splitter, int *range)
{
	RECT rect;
	GetClientRect(splitter, &rect);
	range[0] = SPLIT_LINE_SIZE/2;
	if (GetWindowLong(splitter, GWL_STYLE) & CCS_VERT)
		range[1] = (rect.bottom-rect.top)-SPLIT_LINE_SIZE/2;
	else
		range[1] = (rect.right-rect.left)-SPLIT_LINE_SIZE/2;
}

void osSetSplitterPosition(WindowHandle splitter, int pos)
{
	SplitterData *pData = (SplitterData *) GetWindowLongPtr(splitter, GWLP_USERDATA);
	UpdateSplitterPos(splitter, pData, pos);
}

int osGetSplitterPosition(WindowHandle splitter)
{
	SplitterData *pData = (SplitterData *) GetWindowLongPtr(splitter, GWLP_USERDATA);
	return pData->nSplitterPos;
}
