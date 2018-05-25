#include "CheckBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateCheckBox(WindowHandle window)
{
	printf("osCreateCheckBox\n");
	return NULL;
};

void osGetCheckBoxReqSize(WindowHandle checkbox, int *res)
{
	printf("osGetCheckBoxReqSize\n");
};

char *osGetCheckBoxText(WindowHandle button)
{
	printf("osGetCheckBoxText\n");
	return NULL;
};

void osSetCheckBoxText(WindowHandle button, char *txt)
{
	printf("osSetCheckBoxText\n");
};

BOOL osGetCheckBoxState(WindowHandle checkbox)
{
	printf("osGetCheckBoxState\n");
	return 0;
};

void osSetCheckBoxState(WindowHandle checkbox, BOOL state)
{
	printf("osSetCheckBoxState\n");
};
