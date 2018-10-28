#include "GroupBox.h"
#include "Internals.h"

WNDPROC DefGroupBoxProc = NULL;

extern LRESULT CALLBACK HWindowSharedFunction(WNDPROC pDefWindowProc, HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

LRESULT CALLBACK HGroupBoxFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	WindowData *pData = (WindowData *) GetWindowLongPtrW(hWnd,GWLP_USERDATA);
	
	switch (uMsg)
	{
	case WM_PAINT:
		return CallWindowProc(DefGroupBoxProc, hWnd, uMsg, wParam, lParam);
	case WM_ERASEBKGND:
		{
			HDC hDC = (HDC) wParam;
			RECT rect;
			HRGN hRgn;
			HWND hCtrl;
			
			hCtrl = hWnd;
			while (hCtrl && !pData->hBackBrush)
			{
				hCtrl = GetParent(hCtrl);
				pData = (WindowData *) GetWindowLongPtrW(hCtrl,GWLP_USERDATA);
            }

			GetClipBox(hDC, &rect);
			hRgn = CreateRectRgn(rect.left, rect.top, rect.right, rect.bottom);
			GetClipRgn(hDC, hRgn);
			getWindowClipRgn(hWnd, hRgn);
			FillRgn(hDC, hRgn, pData->hBackBrush);
            DeleteObject(hRgn);
		}
		return TRUE;
	};

	return HWindowSharedFunction(DefGroupBoxProc, hWnd, uMsg, wParam, lParam);
}

WindowHandle osCreateGroupBox(WindowHandle form)
{
    HWND hWnd;

	hWnd = CreateWindowW(
				L"HGROUPBOX",
				NULL,
				WS_CHILD | BS_GROUPBOX,
				0,0,0,0,
				form,
				NULL,
				ghModule,
				NULL
				);
	return checkWindow(hWnd, "HGROUPBOX");
};

void osGetGroupBoxBordersSize(WindowHandle box, int *res)
{
	SIZE sz;
	HDC hDC = GetDC(box);
	HFONT hFont = (HFONT) SendMessage(box,WM_GETFONT,0,0);
	int nLen = GetWindowTextLength(box);
	char *buffer = (char *) rmalloc(nLen+1);
	nLen = GetWindowText(box, buffer, nLen+1);

	if (hFont)
		SelectObject(hDC, hFont);
	GetTextExtentPoint32(hDC, buffer, nLen, &sz);

	rfree(buffer);
 	ReleaseDC(box, hDC);

	res[0] = 5;
	res[1] = max(10,sz.cy+2);
	res[2] = 5;
	res[3] = 5;
}

PortString osGetGroupBoxText(WindowHandle box)
{
	int nLen = GetWindowTextLengthW(box);
	PortString buffer = (PortString) rmalloc((nLen+1)*sizeof(wchar_t));
	GetWindowTextW(box, buffer, nLen+1);
	return buffer;
};

void osSetGroupBoxText(WindowHandle box, PortString txt)
{
	SetWindowTextW(box, txt);
};
