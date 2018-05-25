#include "TrackBar.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateHorzTrackBar(WindowHandle form)
{
	HWND hTrackBar;

	hTrackBar = CreateWindow(
			  UPDOWN_CLASS,
			  NULL,
			  UDS_HORZ | WS_CHILD | WS_TABSTOP,
			  0,0,0,0,
			  form,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hTrackBar, TRACKBAR_CLASS);
};

WindowHandle osCreateVertTrackBar(WindowHandle form)
{
	HWND hTrackBar;

	hTrackBar = CreateWindow(
			  UPDOWN_CLASS,
			  NULL,
			  WS_CHILD | WS_TABSTOP,
			  0,0,0,0,
			  form,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hTrackBar, TRACKBAR_CLASS);
};

void osGetTrackBarReqSize(WindowHandle trackBar, int *res)
{
	if (GetWindowLong(trackBar,GWL_STYLE) & UDS_HORZ)
	{
		res[0] = 20;
		res[1] = GetSystemMetrics(SM_CYHSCROLL);
	}
	else
	{
		res[0] = GetSystemMetrics(SM_CXVSCROLL);
		res[1] = 20;
	}
}
