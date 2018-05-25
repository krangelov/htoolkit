#include "PopUp.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreatePopUp(WindowHandle window)
{
	HWND hPopUp;

	hPopUp = CreateWindow(
			  "COMBOBOX",
			  NULL,
			  WS_CHILD | CBS_DROPDOWNLIST | WS_VSCROLL | WS_TABSTOP,
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hPopUp, "POPUP");
};

void osAppendPopUpItem(WindowHandle popup, char *title)
{
	SendMessage(popup, CB_ADDSTRING, 0, (LPARAM) title);
	osForceContainerReLayout(popup);
};

void osInsertPopUpItem(WindowHandle popup, int index, char *title)
{
	SendMessage(popup, CB_INSERTSTRING, index, (LPARAM) title);
	osForceContainerReLayout(popup);
};

void osRemovePopUpItem(WindowHandle popup, int index)
{
	SendMessage(popup, CB_DELETESTRING, index, 0);
	osForceContainerReLayout(popup);
};

void osRemoveAllPopUpItems(WindowHandle popup)
{
	SendMessage(popup, CB_RESETCONTENT,0,0);
	osForceContainerReLayout(popup);
};

void osGetPopUpReqSize(WindowHandle popup, int *res)
{
	SIZE sz;
	int i,nCount, nLen, width;
	char *buffer;
	HDC hDC = GetDC(popup);

	width = 10;
	nCount = SendMessage(popup,CB_GETCOUNT,0,0);
	for (i = 0; i < nCount; i++)
	{
		nLen = SendMessage(popup,CB_GETLBTEXTLEN,i,0);
		buffer = (char *) rmalloc(nLen+1);
		nLen = SendMessage(popup,CB_GETLBTEXT,i,(LPARAM)buffer);

		GetTextExtentPoint32(hDC, buffer, nLen, &sz);

		rfree(buffer);

		if (width < sz.cx) width = sz.cx;
	}

	ReleaseDC(popup, hDC);

	res[0] = width + GetSystemMetrics(SM_CXVSCROLL);
	res[1] = 20;
};

int osGetPopUpSelection(WindowHandle popup)
{
	return SendMessage(popup, CB_GETCURSEL, 0, 0);
};

void osSetPopUpSelection(WindowHandle popup, int index)
{
	SendMessage(popup, CB_SETCURSEL, index, 0);
	handleControlCommand(popup);
};
