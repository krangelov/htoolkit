#ifndef LISTBOX_H
#define LISTBOX_H

#include "Types.h"

WindowHandle osCreateListBox(WindowHandle window, BOOL multisel);
WindowHandle osCreateCheckListBox(WindowHandle window);
void osAppendListBoxItem(WindowHandle listbox, PortString title);
void osInsertListBoxItem(WindowHandle listbox, int index, PortString title);
void osRemoveListBoxItem(WindowHandle listbox, int index);
void osRemoveAllListBoxItems(WindowHandle listbox);
void osGetListBoxReqSize(WindowHandle listbox, int *res);
int osGetListBoxSingleSelection(WindowHandle listbox);
BOOL osGetListBoxItemSelectState(WindowHandle listbox, int index);
void osSetListBoxSingleSelection(WindowHandle listbox, int index);
void osSetListBoxItemSelectState(WindowHandle listbox, int index, BOOL state);
int osGetListBoxCurrentItem(WindowHandle listbox);

#endif
