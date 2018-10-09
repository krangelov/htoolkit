#ifndef RADIOBOX_H
#define RADIOBOX_H

#include "Types.h"

WindowHandle osCreateRadioBox(WindowHandle window);
void osGetRadioBoxReqSize(WindowHandle radiobox, int *res);
PortString osGetRadioBoxText(WindowHandle checkbox);
void osSetRadioBoxText(WindowHandle checkbox, PortString txt);
BOOL osGetRadioBoxState(WindowHandle radiobox);
void osSetRadioBoxState(WindowHandle radiobox, BOOL state);
void osSetRadioBoxGroup(WindowHandle *handles);

#endif
