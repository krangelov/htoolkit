#ifndef PROGRESSBAR_H
#define PROGRESSBAR_H

#include "Types.h"

WindowHandle osCreateHorzProgressBar(WindowHandle form, BOOL bSmooth);
WindowHandle osCreateVertProgressBar(WindowHandle form, BOOL bSmooth);
void osGetProgressBarReqSize(WindowHandle bar, int *res);
void osSetProgressBarFraction(WindowHandle bar, int minPos, int maxPos, int pos);
int osGetProgressBarFraction(WindowHandle bar, int minPos, int maxPos);

#endif
