#ifndef STATUSBAR_H
#define STATUSBAR_H

#include "Types.h"

void osSetStatusBarVisible(BOOL visible);
BOOL osGetStatusBarVisible();

void osPushStatusBarContext(char *title);
void osPopStatusBarContext();

char *osGetStatusBarTitle();
void osSetStatusBarTitle(char *title);

int osGetStatusBarIndicatorsCount();

IndicatorHandle osCreateIndicator(int index);
void osDestroyIndicator(IndicatorHandle indicator);
char *osGetIndicatorTitle(IndicatorHandle indicator);
void osSetIndicatorTitle(IndicatorHandle indicator, char *title);
int osGetIndicatorPos(IndicatorHandle indicator);

#endif
