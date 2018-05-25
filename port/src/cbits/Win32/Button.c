#include "Button.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateButton(WindowHandle form)
{
	HWND hButton;

	hButton = CreateWindow(
			  "BUTTON",
			  NULL,
			  WS_CHILD | WS_TABSTOP,
			  0,0,0,0,
			  form,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hButton, "BUTTON");
};

void osGetButtonReqSize(WindowHandle button, int *res)
{
	SIZE sz;
	HDC hDC = GetDC(button);
	HFONT hFont = (HFONT) SendMessage(button,WM_GETFONT,0,0);
	int nLen = GetWindowTextLength(button);
	char *buffer = (char *) rmalloc(nLen+1);
	nLen = GetWindowText(button, buffer, nLen+1);

	if (hFont)
		SelectObject(hDC, hFont);
	GetTextExtentPoint32(hDC, buffer, nLen, &sz);

	rfree(buffer);
 	ReleaseDC(button, hDC);

	res[0] = sz.cx + GetSystemMetrics(SM_CXBORDER)*2 + 12;
	res[1] = sz.cy + GetSystemMetrics(SM_CYBORDER)*2 + 6;
}

char *osGetButtonText(WindowHandle button)
{
	int nLen = GetWindowTextLength(button);
	char *buffer = (char *) rmalloc(nLen+1);
	GetWindowText(button, buffer, nLen+1);
	return buffer;
};

void osSetButtonText(WindowHandle button, char *txt)
{
	SetWindowText(button, txt);
	osForceContainerReLayout(button);
};

void osChangeButtonFont(WindowHandle button, FontHandle font)
{
	SendMessage(button, WM_SETFONT, (WPARAM) font, MAKELPARAM (TRUE,0));
	osForceContainerReLayout(button);
};
