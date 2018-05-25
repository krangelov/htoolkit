#include "Label.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateLabel(WindowHandle window)
{
	HWND hText;

	hText = CreateWindow(
			  "STATIC",
			  NULL,
			  WS_CHILD,
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hText, "TEXT");
};

void osGetLabelReqSize(WindowHandle label, int *res)
{
	SIZE sz;
	HDC hDC = GetDC(label);
	HFONT hFont = (HFONT) SendMessage(label,WM_GETFONT,0,0);
	int nLen = GetWindowTextLength(label);
	char *buffer = (char *) rmalloc(nLen+1);
	nLen = GetWindowText(label, buffer, nLen+1);

	if (hFont)
		SelectObject(hDC, hFont);
	GetTextExtentPoint32(hDC, buffer, nLen, &sz);

	rfree(buffer);
 	ReleaseDC(label, hDC);

	res[0] = sz.cx;
	res[1] = sz.cy;
}

char *osGetLabelText(WindowHandle label)
{
	int nLen = GetWindowTextLength(label);
	char *buffer = (char *) rmalloc(nLen+1);
	GetWindowText(label, buffer, nLen+1);
	return buffer;
};

void osSetLabelText(WindowHandle label, char *txt)
{
	SetWindowText(label, txt);
	osForceContainerReLayout(label);
};

void osChangeLabelFont(WindowHandle label, FontHandle font)
{
	SendMessage(label, WM_SETFONT, (WPARAM) font, MAKELPARAM (TRUE,0));
	osForceContainerReLayout(label);
};
