#ifndef CHECKBOX_H
#define CHECKBOX_H

#include "Types.h"

WindowHandle osCreateCheckBox(WindowHandle window);
void osGetCheckBoxReqSize(WindowHandle checkbox, int *res);
PortString osGetCheckBoxText(WindowHandle checkbox);
void osSetCheckBoxText(WindowHandle checkbox, PortString txt);
BOOL osGetCheckBoxState(WindowHandle checkbox);
void osSetCheckBoxState(WindowHandle checkbox, BOOL state);

#endif
