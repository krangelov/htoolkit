#ifndef SPLITTER_H
#define SPLITTER_H

#include "Types.h"

WindowHandle osCreateSplitter(WindowHandle window, BOOL isVert, /* out */ WindowHandle *panes);
void osGetSplitterReqSize(WindowHandle splitter, int *res);
void osGetSplitterRange(WindowHandle splitter, int *range);
void osSetSplitterPosition(WindowHandle splitter, int pos);
int osGetSplitterPosition(WindowHandle splitter);

#endif
