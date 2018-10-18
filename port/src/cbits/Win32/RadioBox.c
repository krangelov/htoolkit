#include "RadioBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateRadioBox(WindowHandle window)
{
	HWND hRadioBox;

	hRadioBox = CreateWindowW(
			  L"BUTTON",
			  NULL,
			  WS_CHILD | BS_AUTORADIOBUTTON | WS_TABSTOP,
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);
	SetWindowLongPtr(hRadioBox, GWLP_USERDATA, (LONG_PTR) hRadioBox);
	return checkWindow(hRadioBox, "RADIOBOX");
};

void osGetRadioBoxReqSize(WindowHandle radiobox, int *res)
{
	SIZE sz;
	TEXTMETRIC tm;
	HFONT hOldFont;
	HDC hDC = GetDC(radiobox);
	int nLen = GetWindowTextLength(radiobox);
	char *buffer = (char *) rmalloc(nLen+1);
	nLen = GetWindowText(radiobox, buffer, nLen+1);

	hOldFont = SelectObject(hDC, (HFONT) SendMessage(radiobox, WM_GETFONT, 0, 0));
	
	GetTextExtentPoint32(hDC, buffer, nLen, &sz);
	GetTextMetrics(hDC, &tm);
	
	SelectObject(hDC, hOldFont);

	rfree(buffer);
	ReleaseDC(radiobox, hDC);

	res[0] = sz.cx + 32;
	res[1] = tm.tmHeight+tm.tmDescent;
};

PortString osGetRadioBoxText(WindowHandle radiobox)
{
	int nLen = GetWindowTextLengthW(radiobox);
	PortString buffer = (PortString) rmalloc((nLen+1)*sizeof(wchar_t));
	GetWindowTextW(radiobox, buffer, nLen+1);
	return buffer;
};

void osSetRadioBoxText(WindowHandle radiobox, PortString txt)
{
	SetWindowTextW(radiobox, txt);
	osForceContainerReLayout(radiobox);
};

BOOL osGetRadioBoxState(WindowHandle radiobox)
{
	return SendMessage(radiobox,BM_GETCHECK,0,0) == BST_CHECKED;
};

void osSetRadioBoxState(WindowHandle radiobox, BOOL state)
{
	HWND hNextCtrl;

	if (state)
	{
		if (SendMessage(radiobox,BM_GETCHECK,0,0) == BST_UNCHECKED)
		{
			SendMessage(radiobox,BM_SETCHECK,BST_CHECKED,0);
			handleControlCommand(radiobox);
		}

		hNextCtrl = radiobox;
		for (;;)
		{
			hNextCtrl = (WindowHandle) GetWindowLongPtr(hNextCtrl, GWLP_USERDATA);
			if (hNextCtrl == radiobox) break;

			if (SendMessage(hNextCtrl,BM_GETCHECK,0,0) == BST_CHECKED)
			{
				SendMessage(hNextCtrl,BM_SETCHECK,BST_UNCHECKED,0);
				handleControlCommand(hNextCtrl);
			}
		}
	}
	else
	{
		if (SendMessage(radiobox,BM_GETCHECK,0,0) == BST_CHECKED)
		{
			SendMessage(radiobox,BM_SETCHECK,BST_UNCHECKED,0);
			handleControlCommand(radiobox);
		}
	}
};

void osSetRadioBoxGroup(WindowHandle *handles)
{
	WindowHandle first, next, child, handle, *phandle;

	if (!(handles && *handles))
		return;

	phandle=handles;
	for (;;)
	{
		handle = *phandle;

		first = (WindowHandle) GetWindowLongPtr(handle, GWLP_USERDATA);
		child = first;
		for (;;)
		{
			next = (WindowHandle) GetWindowLongPtr(child, GWLP_USERDATA);
			if (next == handle) break;
			child = next;
		}
		SetWindowLongPtr(child, GWLP_USERDATA, (LONG_PTR) first);

		if (SendMessage(handle,BM_GETCHECK,0,0) == BST_CHECKED)
		{
			if (handle != *handles)
			{
				SendMessage(handle,BM_SETCHECK,BST_UNCHECKED,0);
				handleControlCommand(handle);
			}
		}
		else
		{
			if (handle == *handles)
			{
				SendMessage(handle,BM_SETCHECK,BST_CHECKED,0);
				handleControlCommand(handle);
			}
		}

		phandle++;
		if (*phandle)
			SetWindowLongPtr(handle, GWLP_USERDATA, (LONG_PTR) *phandle);
		else
		{
			SetWindowLongPtr(handle, GWLP_USERDATA, (LONG_PTR) *handles);
			break;
		}
	}
};
