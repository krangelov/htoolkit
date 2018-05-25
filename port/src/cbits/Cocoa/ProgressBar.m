#include "ProgressBar.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateHorzProgressBar(WindowHandle window, BOOL bSmooth)
{
	printf("osCreateHorzProgressBar\n");
	return NULL;
};

WindowHandle osCreateVertProgressBar(WindowHandle window, BOOL bSmooth)
{
	printf("osCreateVertProgressBar\n");
	return NULL;
};

void osGetProgressBarReqSize(WindowHandle bar, int *res)
{
	printf("osGetProgressBarReqSize\n");
}

void osSetProgressBarFraction(WindowHandle bar, int minPos, int maxPos, int pos)
{
	printf("osSetProgressBarFraction\n");
}

int osGetProgressBarFraction(WindowHandle bar, int minPos, int maxPos)
{
	printf("osGetProgressBarFraction\n");
	return 0;
}
