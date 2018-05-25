#include "Message.h"
#include "Internals.h"

void osMessageAlert(char *szText)
{
	printf("osMessageAlert\n");
};

BOOL osMessageConfirm(char *szText)
{
	printf("osMessageConfirm\n");
	return 0;
};

void osMessageWarning(char *szText)
{
	printf("osMessageWarning\n");
};

BOOL osMessageQuestion(char *szText)
{
	printf("osMessageQuestion\n");
	return 0;
};

BOOL osMessageError(char *szText)
{
	printf("osMessageError\n");
	return 0;
};

int osMessageCancelQuestion(char *szText)
{
	printf("osMessageCancelQuestion\n");
	return 0;
};

int osMessageConfirmSave(char *szText)
{
	printf("osMessageConfirmSave\n");
	return 0;
}
