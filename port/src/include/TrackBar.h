#ifndef UPDOWN_H
#define UPDOWN_H

#include "Types.h"

WindowHandle osCreateHorzTrackBar(WindowHandle form);
WindowHandle osCreateVertTrackBar(WindowHandle form);
void osGetTrackBarReqSize(WindowHandle trackBar, int *res);

#endif
