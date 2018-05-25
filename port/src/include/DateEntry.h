#ifndef DATEENTRY_H
#define DATEENTRY_H

#include "Types.h"

WindowHandle osCreateDateEntry(WindowHandle window);
void osGetDateEntryReqSize(WindowHandle entry, int *res);
time_t osGetDateEntryValue(WindowHandle entry);
void osSetDateEntryValue(WindowHandle entry, time_t value);

#endif
