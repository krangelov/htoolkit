#include "Slider.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateHorzSlider(WindowHandle form)
{
	HWND hSlider;

	hSlider = CreateWindow(
			  TRACKBAR_CLASS,
			  NULL,
			  TBS_AUTOTICKS | TBS_HORZ | WS_CHILD | WS_TABSTOP,
			  0,0,0,0,
			  form,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hSlider, TRACKBAR_CLASS);
};

WindowHandle osCreateVertSlider(WindowHandle form)
{
	HWND hSlider;

	hSlider = CreateWindow(
			  TRACKBAR_CLASS,
			  NULL,
			  TBS_AUTOTICKS | TBS_VERT | WS_CHILD | WS_TABSTOP,
			  0,0,0,0,
			  form,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hSlider, TRACKBAR_CLASS);
};

void osGetSliderReqSize(WindowHandle slider, int *res)
{
	if (GetWindowLongPtrW(slider,GWL_STYLE) & TBS_HORZ)
	{
		res[0] = 32;
		res[1] = GetSystemMetrics(SM_CYHSCROLL);
	}
	else
	{
		res[0] = GetSystemMetrics(SM_CXVSCROLL);
		res[1] = 32;
	}
}

void osSetSliderRange(WindowHandle slider, int minPos, int maxPos)
{
	SendMessage(slider, TBM_SETRANGEMAX, FALSE, maxPos);
	SendMessage(slider, TBM_SETRANGEMIN, TRUE,  minPos);
}

void osGetSliderRange(WindowHandle slider, int *minPos, int *maxPos)
{
	*maxPos = SendMessage(slider, TBM_GETRANGEMAX, 0, 0);
	*minPos = SendMessage(slider, TBM_GETRANGEMIN, 0, 0);
}

int osGetSliderPosition(WindowHandle slider)
{
	return SendMessage(slider, TBM_GETPOS, 0, 0);
}

void osSetSliderPosition(WindowHandle slider, int pos)
{
	SendMessage(slider, TBM_SETPOS, TRUE, pos);
}
