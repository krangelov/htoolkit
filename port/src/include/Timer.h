#ifndef TIMER_H
#define TIMER_H

#include "Types.h"

TimerHandle osCreateTimer(int msecs);
void osDestroyTimer(TimerHandle timer);
void osSetTimerInterval(TimerHandle timer, int msecs);
int  osGetTimerInterval(TimerHandle timer);
void osSetTimerEnabled(TimerHandle timer, BOOL enabled);
BOOL osGetTimerEnabled(TimerHandle timer);

#endif
