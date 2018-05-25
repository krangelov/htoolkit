#include "GroupBox.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include "Window.h"

WindowHandle osCreateGroupBox(WindowHandle window)
{
	printf("osCreateGroupBox\n");
	return NULL;
};

void osGetGroupBoxBordersSize(WindowHandle groupbox, int *res)
{
	printf("osGetGroupBoxBordersSize\n");
}

char *osGetGroupBoxText(WindowHandle groupbox)
{
	printf("osGetGroupBoxText\n");
	return NULL;
};

void osSetGroupBoxText(WindowHandle groupbox, char *txt)
{
	printf("osSetGroupBoxText\n");
};
