#ifndef POPUP_H
#define POPUP_H

#include "Types.h"

WindowHandle osCreatePopUp(WindowHandle window);
void osAppendPopUpItem(WindowHandle popup, char *title);
void osInsertPopUpItem(WindowHandle popup, int index, char *title);
void osRemovePopUpItem(WindowHandle popup, int index);
void osRemoveAllPopUpItems(WindowHandle popup);
void osGetPopUpReqSize(WindowHandle popup, int *res);
int osGetPopUpSelection(WindowHandle popup);
void osSetPopUpSelection(WindowHandle popup, int index);

#endif
