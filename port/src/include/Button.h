#ifndef BUTTON_H
#define BUTTON_H

#include "Types.h"

WindowHandle osCreateButton(WindowHandle window);
void osGetButtonReqSize(WindowHandle button, int *res);
char *osGetButtonText(WindowHandle button);
void osSetButtonText(WindowHandle button, char *txt);
void osChangeButtonFont(WindowHandle button, FontHandle font);

#endif
