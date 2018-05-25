#ifndef ACTION_H
#define ACTION_H

#include "Types.h"

ActionHandle osCreateAction();
ActionHandle osCreateCheckAction();
ActionHandle osCreateRadioAction();
ActionHandle osCreateDropDownAction(MenuHandle menu);
void osSetActionRadioGroup(ActionHandle *handles);
void osSetActionBitmap(ActionHandle action, BitmapHandle bitmap);
void osSetActionEnabled(ActionHandle action, BOOL enabled);
BOOL osGetActionEnabled(ActionHandle toolButton);
void osSetActionTip(ActionHandle action, char *text);
char *osGetActionTip(ActionHandle action);
void osSetActionText(ActionHandle action, char *text);
char *osGetActionText(ActionHandle action);
void osSetActionShortText(ActionHandle action, char *text);
char *osGetActionShortText(ActionHandle action);
void osSetActionChecked(ActionHandle action, BOOL checked);
BOOL osGetActionChecked(ActionHandle action);
void osSetActionAccel(ActionHandle action, int key, unsigned int mods);
void osGetActionAccel(ActionHandle action, int *key, unsigned int *mods);
void osDestroyAction(ActionHandle action);
void osDestroyAllActions();

#endif

