#ifndef MESSAGE_H
#define MESSAGE_H

#include "Types.h"

void osMessageInfo(char *szText);
BOOL osMessageConfirm(char *szText);
void osMessageWarning(char *szText);
BOOL osMessageQuestion(char *szText);
BOOL osMessageError(char *szText);
int osMessageCancelQuestion(char *szText);
int osMessageConfirmSave(char *szText);

#endif
