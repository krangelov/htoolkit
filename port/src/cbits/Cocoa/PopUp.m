#include "PopUp.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreatePopUp(WindowHandle window)
{
	printf("osCreatePopUp\n");
	return NULL;
};

void osAppendPopUpItem(WindowHandle popup, char *title)
{
	printf("osAppendPopUpItem\n");
};

void osInsertPopUpItem(WindowHandle popup, int index, char *title)
{
	printf("osInsertPopUpItem\n");
};

void osRemovePopUpItem(WindowHandle popup, int index)
{
	printf("osRemovePopUpItem\n");
};

void osRemoveAllPopUpItems(WindowHandle popup)
{
	printf("osRemoveAllPopUpItems\n");
};

void osGetPopUpReqSize(WindowHandle popup, int *res)
{
	printf("osGetPopUpReqSize\n");
};

int osGetPopUpSelection(WindowHandle popup)
{
	printf("osGetPopUpSelection\n");
	return 0;
};

void osSetPopUpSelection(WindowHandle popup, int index)
{
	printf("osSetPopUpSelection\n");
};
