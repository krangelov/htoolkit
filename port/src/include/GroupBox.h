#ifndef GROUPBOX_H
#define GROUPBOX_H

#include "Types.h"

WindowHandle osCreateGroupBox(WindowHandle form);
void osGetGroupBoxBordersSize(WindowHandle box, int *res);
char *osGetGroupBoxText(WindowHandle box);
void osSetGroupBoxText(WindowHandle box, char *txt);

#endif
