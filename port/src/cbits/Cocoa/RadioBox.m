#include "RadioBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateRadioBox(WindowHandle window)
{
	printf("osCreateRadioBox\n");
	return NULL;
};

void osGetRadioBoxReqSize(WindowHandle radio, int *res)
{
	printf("osGetRadioBoxReqSize\n");
};

char *osGetRadioBoxText(WindowHandle button)
{
	printf("osGetRadioBoxText\n");
	return NULL;
};

void osSetRadioBoxText(WindowHandle button, char *txt)
{
	printf("osSetRadioBoxText\n");
};

BOOL osGetRadioBoxState(WindowHandle radio)
{
	printf("osGetRadioBoxState\n");
	return 0;
};

void osSetRadioBoxState(WindowHandle radio, BOOL state)
{
	printf("osSetRadioBoxState\n");
};

void osSetRadioBoxGroup(WindowHandle *handles)
{
	printf("osSetRadioBoxGroup\n");
}
