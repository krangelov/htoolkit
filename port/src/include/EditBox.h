#ifndef EDITBOX_H
#define EDITBOX_H

#include "Types.h"

WindowHandle osCreateEdit(WindowHandle window);
void osGetEditReqSize(WindowHandle edit, int *res);
char *osGetEditText(WindowHandle editbox);
void osSetEditText(WindowHandle editbox, char *txt);
void osSetEditReadOnly(WindowHandle editbox, BOOL readOnly);
BOOL osGetEditReadOnly(WindowHandle editbox);
void osSetEditPassword(WindowHandle editbox, BOOL visible);
BOOL osGetEditPassword(WindowHandle editbox);
void osChangeEditBoxFont(WindowHandle editbox, FontHandle font);

#endif
