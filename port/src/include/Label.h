#ifndef LABEL_H
#define LABEL_H

#include "Types.h"

WindowHandle osCreateLabel(WindowHandle window);
void osGetLabelReqSize(WindowHandle label, int *res);
char *osGetLabelText(WindowHandle label);
void osSetLabelText(WindowHandle label, char *txt);
void osChangeLabelFont(WindowHandle label, FontHandle font);

#endif
