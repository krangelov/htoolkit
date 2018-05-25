#include "EditBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateEdit(WindowHandle window)
{
	printf("osCreateEdit\n");
	return NULL;
};

void osGetEditReqSize(WindowHandle edit, int *res)
{
	printf("osGetEditReqSize\n");
}

char *osGetEditText(WindowHandle entry)
{
	printf("osGetEditText\n");
	return NULL;
};

void osSetEditText(WindowHandle entry, char *txt)
{
	printf("osSetEditText\n");
};

void osSetEditReadOnly(WindowHandle entry, BOOL readOnly)
{
	printf("osSetEditReadOnly\n");
}

BOOL osGetEditReadOnly(WindowHandle entry)
{
	printf("osGetEditReadOnly\n");
	return 0;
}

void osSetEditPassword(WindowHandle entry, BOOL password)
{
	printf("osSetEditPassword\n");
}

BOOL osGetEditPassword(WindowHandle entry)
{
	printf("osGetEditPassword\n");
	return 0;
}

void osChangeEditBoxFont(WindowHandle entry, FontHandle font)
{
	printf("osChangeEditBoxFont\n");
};
