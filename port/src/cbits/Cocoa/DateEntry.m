#include "DateEntry.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateDateEntry(WindowHandle window)
{
	printf("osCreateDateEntry\n");
	return NULL;
};

void osGetDateEntryReqSize(WindowHandle entry, int *res)
{
	printf("osGetDateEntryReqSize\n");
}

time_t osGetDateEntryValue(WindowHandle entry)
{
	printf("osGetDateEntryValue\n");
	return 0;
}

void osSetDateEntryValue(WindowHandle entry, time_t value)
{
	printf("osSetDateEntryValue\n");
}
