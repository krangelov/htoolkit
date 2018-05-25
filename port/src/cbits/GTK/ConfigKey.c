#include "ConfigKey.h"
#include "Internals.h"

static char *getKeyPath(char *szName)
{
	char *s;

	s = szName;
	while (*s)
	{
		if (*s == '.') *s = '/';
		s++;
	}

	return gnome_gconf_get_app_settings_relative(gProgram,szName);
}

static GConfValue *getKeyValue(char *szName, int type)
{
	char *key;
	GConfValue* val;

	key = getKeyPath(szName);
	val = gconf_client_get(gconf_client_get_default(), key, NULL);
	free(key);

	return (val == NULL || val->type != type) ? NULL : val;
}


char *osGetConfigStringKey(char *szName, char *defvalue)
{
	char *retval;
	GConfValue *val = getKeyValue(szName, GCONF_VALUE_STRING);

	if (val)
	{
		retval = strdup(gconf_value_get_string(val));
		gconf_value_free (val);
	}
	else
		retval = defvalue ? strdup(defvalue) : NULL;

	return retval;
}

void osSetConfigStringKey(char *szName, char *value)
{
	char *key = getKeyPath(szName);
	gconf_client_set_string(gconf_client_get_default(), key, value, NULL);
	free(key);
}

int osGetConfigIntKey(char *szName, int defvalue)
{
	int retval;
	GConfValue *val = getKeyValue(szName, GCONF_VALUE_INT);

	if (val)
	{
		retval = gconf_value_get_int(val);
		gconf_value_free (val);
	}
	else
		retval = defvalue;

	return retval;
}

void osSetConfigIntKey(char *szName, int value)
{
	char *key = getKeyPath(szName);
	gconf_client_set_int(gconf_client_get_default(), key, value, NULL);
	free(key);
}

double osGetConfigDoubleKey(char *szName, double defvalue)
{
	double retval;
	GConfValue *val = getKeyValue(szName, GCONF_VALUE_FLOAT);

	if (val)
	{
		retval = gconf_value_get_float(val);
		gconf_value_free (val);
	}
	else
		retval = defvalue;

	return retval;
}

void osSetConfigDoubleKey(char *szName, double value)
{
	char *key = getKeyPath(szName);
	gconf_client_set_float(gconf_client_get_default(), key, value, NULL);
	free(key);
}

BOOL osGetConfigBoolKey(char *szName, BOOL defvalue)
{
	BOOL retval;
	GConfValue *val = getKeyValue(szName, GCONF_VALUE_BOOL);

	if (val)
	{
		retval = gconf_value_get_bool(val);
		gconf_value_free (val);
	}
	else
		retval = defvalue;

	return retval;
}

void osSetConfigBoolKey(char *szName, BOOL value)
{
	char *key = getKeyPath(szName);
	gconf_client_set_bool(gconf_client_get_default(), key, value, NULL);
	free(key);
}
