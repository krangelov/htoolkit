#ifndef CHECKBOX_H
#define CHECKBOX_H

#include "Types.h"

WindowHandle osCreateCheckBox(WindowHandle window);
void osGetCheckBoxReqSize(WindowHandle checkbox, int *res);
char *osGetCheckBoxText(WindowHandle checkbox);
void osSetCheckBoxText(WindowHandle checkbox, char *txt);
BOOL osGetCheckBoxState(WindowHandle checkbox);
void osSetCheckBoxState(WindowHandle checkbox, BOOL state);

#endif
