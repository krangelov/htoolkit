#include "StatusBar.h"
#include "Internals.h"
#include "Handlers_stub.h"

void osSetStatusBarVisible(BOOL visible)
{
	// Always visible on MacOS
}

BOOL osGetStatusBarVisible()
{
	return TRUE;
}

void osPushStatusBarContext(char *title)
{
	printf("osPushStatusBarContext\n");
}

void osPopStatusBarContext()
{
	printf("osPopStatusBarContext\n");
}

char *osGetStatusBarTitle()
{
	printf("osGetStatusBarTitle\n");
	return NULL;
}

void osSetStatusBarTitle(char *title)
{
	printf("osSetStatusBarTitle\n");
}

int osGetStatusBarIndicatorsCount()
{
	printf("osGetStatusBarIndicatorsCount\n");
	return 0;
}

IndicatorHandle osCreateIndicator(int index)
{
	return [[NSStatusBar systemStatusBar] statusItemWithLength: 10];
}

void osDestroyIndicator(IndicatorHandle indicator)
{
	printf("osDestroyIndicator\n");
}

char *osGetIndicatorTitle(IndicatorHandle indicator)
{
	printf("osGetIndicatorTitle\n");
	return NULL;
}

void osSetIndicatorTitle(IndicatorHandle indicator, char *title)
{
	printf("osSetIndicatorTitle\n");
}

int osGetIndicatorPos(IndicatorHandle indicator)
{
	printf("osGetIndicatorPos\n");
	return 0;
}
