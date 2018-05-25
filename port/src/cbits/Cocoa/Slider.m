#include "Slider.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateHorzSlider(WindowHandle window)
{
	printf("osCreateHorzSlider\n");
	return NULL;
};

WindowHandle osCreateVertSlider(WindowHandle window)
{
	printf("osCreateVertSlider\n");
	return NULL;
};

void osGetSliderReqSize(WindowHandle slider, int *res)
{
	printf("osGetSliderReqSize\n");
}

void osSetSliderRange(WindowHandle slider, int minPos, int maxPos)
{
	printf("osSetSliderRange\n");
}

void osGetSliderRange(WindowHandle slider, int *minPos, int *maxPos)
{
	printf("osGetSliderRange\n");
}

void osSetSliderPosition(WindowHandle slider, int pos)
{
	printf("osSetSliderPosition\n");
}

int osGetSliderPosition(WindowHandle slider)
{
	printf("osGetSliderPosition\n");
	return 0;
}
