#ifndef NOTEBOOK_H
#define NOTEBOOK_H

#include "Types.h"

WindowHandle osCreateNotebook(WindowHandle form);
void osGetNotebookReqSize(WindowHandle notebook, int *res);
void osSetNotebookLabelsPosition(WindowHandle notebook, PositionType position);
PositionType osGetNotebookLabelsPosition(WindowHandle notebook);
int osGetNotebookSelection(WindowHandle notebook);
void osSetNotebookSelection(WindowHandle notebook, int index);
int osGetNotebookPageCount(WindowHandle notebook);
WindowHandle osInsertNotebookPage(WindowHandle notebook, int pos);
char *osGetNotebookPageTitle(WindowHandle window);
void osSetNotebookPageTitle(WindowHandle window, char *txt);
int osGetNotebookPagePos(WindowHandle handle);
void osDestroyNotebookPage(WindowHandle window);
void osGetNotebookPageSize(WindowHandle window, int *res);
void osSetNotebookPageBitmap(WindowHandle window, BitmapHandle bitmap);

#endif

