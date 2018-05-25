#ifndef CONFIGKEY_H
#define CONFIGKEY_H

#include "Types.h"

char *osGetConfigStringKey(char *name, char *defvalue);
void osSetConfigStringKey(char *name, char *value);
int osGetConfigIntKey(char *szName, int defvalue);
void osSetConfigIntKey(char *szName, int value);
double osGetConfigDoubleKey(char *szName, double defvalue);
void osSetConfigDoubleKey(char *szName, double value);
BOOL osGetConfigBoolKey(char *szName, BOOL defvalue);
void osSetConfigBoolKey(char *szName, BOOL value);

#endif
