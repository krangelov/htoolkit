#include "Timer.h"
#include "Handlers_stub.h"
#include "Internals.h"

TimerHandle osCreateTimer(int msecs)
{
	NSTimer *timer = 
		[[NSTimer alloc] initWithFireDate: NULL
		                         interval: msecs*1000
		                           target: [NSApp delegate]
		                         selector: @selector(timerClicked:)
		                         userInfo: NULL
		                          repeats: YES];
	return timer;
}

void osDestroyTimer(TimerHandle timer)
{
	printf("osDestroyTimer\n");
}

void osSetTimerInterval(TimerHandle timer, int msecs)
{
	printf("osSetTimerInterval\n");
};

int osGetTimerInterval(TimerHandle timer)
{
	printf("osGetTimerInterval\n");
	return 0;
};

void osSetTimerEnabled(TimerHandle timer, BOOL enabled)
{
	printf("osSetTimerEnabled\n");
};

BOOL osGetTimerEnabled(TimerHandle timer)
{
	printf("osGetTimerEnabled\n");
	return 0;
};
