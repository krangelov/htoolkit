#include "Timer.h"
#include "Handlers_stub.h"
#include "Internals.h"

static HWND ghTimerWnd = NULL;

static LRESULT CALLBACK HTimerWindowFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_TIMER:
		{
			TimerHandle timer = (TimerHandle) wParam;
			if (timer->enabled) handleTimer(timer);
		}
		return 0;
	default:
		return DefWindowProc(hWnd, uMsg, wParam, lParam);
	}
}

TimerHandle osCreateTimer(int msecs)
{
	TimerHandle timer;

	if (!ghTimerWnd)
	{
		WNDCLASS wc;

		wc.style = CS_DBLCLKS;
		wc.lpfnWndProc = HTimerWindowFunction;
		wc.cbClsExtra = 0;
		wc.cbWndExtra = 0;
		wc.hInstance  = ghModule;
		wc.hIcon = NULL;
		wc.hCursor = NULL;
		wc.hbrBackground = NULL;
		wc.lpszMenuName = NULL;
		wc.lpszClassName = "HTIMERWINDOW";
		RegisterClass(&wc);

		ghTimerWnd = CreateWindow(
					  "HTIMERWINDOW",
					  NULL,
					  0,
					  CW_USEDEFAULT,0,0,0,
					  NULL,
					  NULL,
					  ghModule,
					  NULL
					);
	};

	timer = (TimerHandle) rmalloc(sizeof(*timer));
	timer->interval = msecs;
	timer->enabled  = TRUE;
	timer->id = (msecs > 0) ? SetTimer(ghTimerWnd, (WPARAM) timer, msecs, NULL) : 0;
	return timer;
}

void osDestroyTimer(TimerHandle timer)
{
	if (timer!=NULL)
	{
		handleTimerDestroy(timer);
		if (timer->id > 0)
		  KillTimer(ghTimerWnd,timer->id);
		rfree(timer);
	}
}

void osSetTimerInterval(TimerHandle timer, int msecs)
{
	if (timer->interval != msecs)
	{
		timer->interval = msecs;
		if (timer->enabled)
		{
			if (timer->id > 0)
				KillTimer(ghTimerWnd,timer->id);
			timer->id = (msecs > 0) ? SetTimer(ghTimerWnd, (WPARAM) timer, msecs, NULL) : 0;
		}
	}
};

int osGetTimerInterval(TimerHandle timer)
{
	return timer->interval;
};

void osSetTimerEnabled(TimerHandle timer, BOOL enabled)
{
	timer->enabled = enabled;
	if (timer->enabled)
	{
		if (timer->id <= 0 && timer->interval > 0)
			timer->id = SetTimer(ghTimerWnd, (WPARAM) timer, timer->interval, NULL);
	}
	else
	{
		if (timer->id > 0)
		  KillTimer(ghTimerWnd,timer->id);
		timer->id = 0;
	}
};

BOOL osGetTimerEnabled(TimerHandle timer)
{
	return timer->enabled;
};
