#ifndef SLIDER_H
#define SLIDER_H

#include "Types.h"

WindowHandle osCreateHorzSlider(WindowHandle form);
WindowHandle osCreateVertSlider(WindowHandle form);
void osGetSliderReqSize(WindowHandle button, int *res);
void osSetSliderRange(WindowHandle slider, int minPos, int maxPos);
void osGetSliderRange(WindowHandle slider, int *minPos, int *maxPos);
int osGetSliderPosition(WindowHandle slider);
void osSetSliderPosition(WindowHandle slider, int pos);

#endif
