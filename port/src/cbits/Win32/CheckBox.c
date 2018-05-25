#include "CheckBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateCheckBox(WindowHandle window)
{
	HWND hCheckBox;

	hCheckBox = CreateWindow(
			  "BUTTON",
			  NULL,
			  WS_CHILD | BS_AUTOCHECKBOX | WS_TABSTOP,
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hCheckBox, "CHECKBOX");
};

void osGetCheckBoxReqSize(WindowHandle checkbox, int *res)
{
	SIZE sz;
	TEXTMETRIC tm;
	HFONT hOldFont;
	HDC hDC = GetDC(checkbox);
	int nLen = GetWindowTextLength(checkbox);
	char *buffer = (char *) rmalloc(nLen+1);
	nLen = GetWindowText(checkbox, buffer, nLen+1);

	hOldFont = SelectObject(hDC, (HFONT) SendMessage(checkbox, WM_GETFONT, 0, 0));
	
	GetTextExtentPoint32(hDC, buffer, nLen, &sz);
	GetTextMetrics(hDC, &tm);
	
	SelectObject(hDC, hOldFont);

	rfree(buffer);
	ReleaseDC(checkbox, hDC);

	res[0] = sz.cx + 20;
	res[1] = tm.tmHeight+tm.tmDescent;
};

char *osGetCheckBoxText(WindowHandle checkbox)
{
	int nLen = GetWindowTextLength(checkbox);
	char *buffer = (char *) rmalloc(nLen+1);
	GetWindowText(checkbox, buffer, nLen+1);
	return buffer;
};

void osSetCheckBoxText(WindowHandle checkbox, char *txt)
{
	SetWindowText(checkbox, txt);
	osForceContainerReLayout(checkbox);
};

BOOL osGetCheckBoxState(WindowHandle checkbox)
{
	return SendMessage(checkbox,BM_GETCHECK,0,0) == BST_CHECKED;
};

void osSetCheckBoxState(WindowHandle checkbox, BOOL state)
{
	SendMessage(checkbox,BM_SETCHECK,state ? BST_CHECKED : BST_UNCHECKED,0);
};
