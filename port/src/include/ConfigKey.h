#ifndef CONFIGKEY_H
#define CONFIGKEY_H

#include "Types.h"

PortString osGetConfigStringKey(PortString name, PortString defvalue);
void osSetConfigStringKey(PortString name, PortString value);
int osGetConfigIntKey(PortString szName, int defvalue);
void osSetConfigIntKey(PortString szName, int value);
double osGetConfigDoubleKey(PortString szName, double defvalue);
void osSetConfigDoubleKey(PortString szName, double value);
BOOL osGetConfigBoolKey(PortString szName, BOOL defvalue);
void osSetConfigBoolKey(PortString szName, BOOL value);

#endif
