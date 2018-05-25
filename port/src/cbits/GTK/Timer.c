#include "Timer.h"
#include "Handlers_stub.h"
#include "Internals.h"

static gboolean osTimerProc(gpointer data)
{
  handleTimer((TimerHandle) data);
  return TRUE;
}

TimerHandle osCreateTimer(int msecs)
{
	TimerHandle timer = rmalloc(sizeof(*timer));
	timer->interval = msecs;
	timer->enabled  = TRUE;
	timer->id = (msecs > 0) ? gtk_timeout_add(msecs, osTimerProc, timer) : 0;
	return timer;
}

void osDestroyTimer(TimerHandle timer)
{
	if (timer!=NULL)
	{
	    handleTimerDestroy(timer);
		if (timer->id > 0)
			gtk_timeout_remove(timer->id);
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
				gtk_timeout_remove(timer->id);
			timer->id = (msecs > 0) ? gtk_timeout_add(msecs, osTimerProc, timer) : 0;
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
			timer->id = gtk_timeout_add(timer->interval, osTimerProc, timer);
	}
	else
	{
		if (timer->id > 0)
		  gtk_timeout_remove(timer->id);
		timer->id = 0;
	}
};

BOOL osGetTimerEnabled(TimerHandle timer)
{
	return timer->enabled;
};
