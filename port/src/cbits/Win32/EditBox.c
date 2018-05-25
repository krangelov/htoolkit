#include "EditBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateEdit(WindowHandle window)
{
	HWND hEdit;

	hEdit = CreateWindowEx(
			  WS_EX_CLIENTEDGE,
			  "EDIT",
			  NULL,
			  WS_CHILD | WS_BORDER | ES_AUTOHSCROLL | WS_TABSTOP,
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);

	return checkWindow(hEdit, "EDIT");
};

void osGetEditReqSize(WindowHandle edit, int *res)
{
	SIZE sz;
	HDC hDC = GetDC(edit);
	HFONT hFont = (HFONT) SendMessage(edit,WM_GETFONT,0,0);
	int nLen     = 1;
	char *buffer = "m";    /* pretend a single letter as a minimal size */

	if (hFont) SelectObject(hDC, hFont);
	GetTextExtentPoint32(hDC, buffer, nLen, &sz);
 	ReleaseDC(edit, hDC);

	res[0] = sz.cx + GetSystemMetrics(SM_CXBORDER)*2 + 6;
	res[1] = sz.cy + GetSystemMetrics(SM_CYBORDER)*2 + 6;
}

char *osGetEditText(WindowHandle editbox)
{
	int nLen = GetWindowTextLength(editbox);
	char *buffer = (char *) rmalloc(nLen+1);
	GetWindowText(editbox, buffer, nLen+1);
	return buffer;
};

void osSetEditText(WindowHandle editbox, char *txt)
{
	SetWindowText(editbox, txt);
};

void osSetEditReadOnly(WindowHandle editbox, BOOL readOnly)
{
	SendMessage(editbox,EM_SETREADONLY,readOnly,0);
}

BOOL osGetEditReadOnly(WindowHandle editbox)
{
	return (GetWindowLong(editbox, GWL_STYLE) & ES_READONLY) != 0;
}

void osSetEditPassword(WindowHandle editbox, BOOL password)
{
	LONG lStyle;

	lStyle = GetWindowLong(editbox, GWL_STYLE);

	if (!password)
	{
		SetWindowLong(editbox, GWL_STYLE,  lStyle & ~ES_PASSWORD);
		SendMessage(editbox, EM_SETPASSWORDCHAR, 0, 0);
	}
	else
	{
		SetWindowLong(editbox, GWL_STYLE,  lStyle | ES_PASSWORD);
		SendMessage(editbox, EM_SETPASSWORDCHAR, (WPARAM) '*', 0);
	}

	InvalidateRect(editbox,NULL,TRUE);
}

BOOL osGetEditPassword(WindowHandle editbox)
{
	return (GetWindowLong(editbox, GWL_STYLE) & ES_PASSWORD) != 0;
}

void osChangeEditBoxFont(WindowHandle editbox, FontHandle font)
{
	SendMessage(editbox, WM_SETFONT, (WPARAM) font, MAKELPARAM (TRUE,0));
	osForceContainerReLayout(editbox);
};
