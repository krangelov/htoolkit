#include "Splitter.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateSplitter(WindowHandle window, BOOL isVert, /* out */ WindowHandle *panes)
{
	printf("osCreateSplitter\n");
	return NULL;
}

void osGetSplitterReqSize(WindowHandle splitter, int *res)
{
	printf("osGetSplitterReqSize\n");
}

void osGetSplitterRange(WindowHandle splitter, int *range)
{
	printf("osGetSplitterRange\n");
}

void osSetSplitterPosition(WindowHandle splitter, int pos)
{
	printf("osSetSplitterPosition\n");
}

int osGetSplitterPosition(WindowHandle splitter)
{
	printf("osGetSplitterPosition\n");
	return 0;
}
