#include "ListBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateListBox(WindowHandle window, BOOL multisel)
{
	printf("osCreateListBox\n");
	return NULL;
};

WindowHandle osCreateCheckListBox(WindowHandle window)
{
	printf("osCreateCheckListBox\n");
	return NULL;
};

void osAppendListBoxItem(WindowHandle listbox, char *title)
{
	printf("osAppendListBoxItem\n");
};

void osInsertListBoxItem(WindowHandle listbox, int index, char *title)
{
	printf("osInsertListBoxItem\n");
};

void osRemoveListBoxItem(WindowHandle listbox, int index)
{
	printf("osRemoveListBoxItem\n");
};

void osRemoveAllListBoxItems(WindowHandle listbox)
{
	printf("osRemoveAllListBoxItems\n");
};

void osGetListBoxReqSize(WindowHandle listbox, int *res)
{
	printf("osGetListBoxReqSize\n");
};

int osGetListBoxSingleSelection(WindowHandle listbox)
{
	printf("osGetListBoxSingleSelection\n");
	return 0;
};

BOOL osGetListBoxItemSelectState(WindowHandle listbox, int index)
{
	printf("osGetListBoxItemSelectState\n");
	return 0;
}

void osSetListBoxSingleSelection(WindowHandle listbox, int index)
{
	printf("osSetListBoxSingleSelection\n");
};

void osSetListBoxItemSelectState(WindowHandle listbox, int index, BOOL state)
{
	printf("osSetListBoxItemSelectState\n");
};

int osGetListBoxCurrentItem(WindowHandle listbox)
{
	printf("osGetListBoxCurrentItem\n");
	return 0;
}
