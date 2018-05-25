#include "Notebook.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateNotebook(WindowHandle window)
{
	printf("osCreateNotebook\n");
	return NULL;
};

void osGetNotebookReqSize(WindowHandle widget, int *res)
{
	printf("osGetNotebookReqSize\n");
}

void osSetNotebookLabelsPosition(WindowHandle notebook, PositionType position)
{
	printf("osSetNotebookLabelsPosition\n");
}

PositionType osGetNotebookLabelsPosition(WindowHandle notebook)
{
	printf("osGetNotebookLabelsPosition\n");
	return 0;
}

int osGetNotebookSelection(WindowHandle notebook)
{
	printf("osGetNotebookSelection\n");
	return 0;
};

void osSetNotebookSelection(WindowHandle notebook, int index)
{
	printf("osSetNotebookSelection\n");
};

WindowHandle osInsertNotebookPage(WindowHandle notebook, int pos)
{
	printf("osInsertNotebookPage\n");
	return NULL;
}

char *osGetNotebookPageTitle(WindowHandle page)
{
	printf("osGetNotebookPageTitle\n");
	return NULL;
};

void osSetNotebookPageTitle(WindowHandle page, char *txt)
{
	printf("osSetNotebookPageTitle\n");
};

int osGetNotebookPagePos(WindowHandle page)
{
	printf("osGetNotebookPagePos\n");
	return 0;
}

void osDestroyNotebookPage(WindowHandle page)
{
	printf("osDestroyNotebookPage\n");
}

void osGetNotebookPageSize(WindowHandle page, int *res)
{
	printf("osGetNotebookPageSize\n");
}

void osSetNotebookPageBitmap(WindowHandle page, BitmapHandle bitmap)
{
	printf("osSetNotebookPageBitmap\n");
}

int osGetNotebookPageCount(WindowHandle notebook)
{
	printf("osGetNotebookPageCount\n");
	return 0;
}
