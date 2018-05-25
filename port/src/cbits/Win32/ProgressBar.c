#include "ProgressBar.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateHorzProgressBar(WindowHandle form, BOOL bSmooth)
{
	HWND hBar;

	hBar = CreateWindow(
			  PROGRESS_CLASS,
			  NULL,
			  (bSmooth ? PBS_SMOOTH : 0) | WS_CHILD | WS_TABSTOP,
			  0,0,0,0,
			  form,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hBar, PROGRESS_CLASS);
};

WindowHandle osCreateVertProgressBar(WindowHandle form, BOOL bSmooth)
{
	HWND hBar;

	hBar = CreateWindow(
			  PROGRESS_CLASS,
			  NULL,
			  (bSmooth ? PBS_SMOOTH : 0) | PBS_VERTICAL | WS_CHILD | WS_TABSTOP,
			  0,0,0,0,
			  form,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hBar, PROGRESS_CLASS);
};

void osGetProgressBarReqSize(WindowHandle bar, int *res)
{
	LONG lStyle = GetWindowLong(bar,GWL_STYLE);

	if (lStyle & PBS_VERTICAL)
	{
		res[0] = GetSystemMetrics(SM_CXVSCROLL);
		res[1] = GetSystemMetrics(SM_CYVSCROLL)*2;
	}
	else
	{
		res[0] = GetSystemMetrics(SM_CXHSCROLL)*2;
		res[1] = GetSystemMetrics(SM_CYHSCROLL);
	}
}

void osSetProgressBarFraction(WindowHandle bar, int minPos, int maxPos, int pos)
{
	SendMessage(bar, PBM_SETRANGE32, minPos, maxPos);
	SendMessage(bar, PBM_SETPOS, pos, 0);
}

int osGetProgressBarFraction(WindowHandle bar, int minPos, int maxPos)
{
	return SendMessage(bar, PBM_GETPOS, 0, 0);
}

