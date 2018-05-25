#include "ConfigKey.h"
#include "Internals.h"

char *osGetConfigStringKey(char *szName, char *defvalue)
{
	printf("osGetConfigStringKey\n");
	return NULL;
}

void osSetConfigStringKey(char *szName, char *value)
{
	printf("osSetConfigStringKey\n");
}

int osGetConfigIntKey(char *szName, int defvalue)
{
	printf("osGetConfigIntKey\n");
	return 0;
}

void osSetConfigIntKey(char *szName, int value)
{
	printf("osSetConfigIntKey\n");
}

double osGetConfigDoubleKey(char *szName, double defvalue)
{
	printf("osGetConfigDoubleKey\n");
	return 0;
}

void osSetConfigDoubleKey(char *szName, double value)
{
	printf("osSetConfigDoubleKey\n");
}

BOOL osGetConfigBoolKey(char *szName, BOOL defvalue)
{
	printf("osGetConfigBoolKey\n");
	return 0;
}

void osSetConfigBoolKey(char *szName, BOOL value)
{
	printf("osSetConfigBoolKey\n");
}
