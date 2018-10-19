#ifndef MESSAGE_H
#define MESSAGE_H

#include "Types.h"

void osMessageInfo(PortString szText);
BOOL osMessageConfirm(PortString szText);
void osMessageWarning(PortString szText);
BOOL osMessageQuestion(PortString szText);
BOOL osMessageError(PortString szText);
int osMessageCancelQuestion(PortString szText);
int osMessageConfirmSave(PortString szText);

#endif
