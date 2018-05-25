#include "ConfigKey.h"
#include "Internals.h"

static HKEY openKey(char *szName, char **pszValueName)
{
	char *s;
	FrameData *pFrameData;
	HKEY hSoftwareKey, hAppKey, hKey;

	pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

	s = szName;
	*pszValueName = szName;
	while (*s)
	{
		if (*s == '.')
		{
			*pszValueName = s+1;
			*s = '\\';
		}

		s++;
	}

	if (RegOpenKeyEx(HKEY_CURRENT_USER, "Software", 0, KEY_READ, &hSoftwareKey) != ERROR_SUCCESS)
		return NULL;

	if (RegCreateKeyEx(hSoftwareKey, pFrameData->lpszAppName, 0, 0, REG_OPTION_NON_VOLATILE, KEY_READ | KEY_WRITE, NULL, &hAppKey, NULL)!= ERROR_SUCCESS)
		return NULL;

	if (*pszValueName == szName)
		return hAppKey;
	else
	{
		(*pszValueName)[-1] = 0;
		if (RegCreateKeyEx(hAppKey, szName, 0, 0, REG_OPTION_NON_VOLATILE, KEY_READ | KEY_WRITE, NULL, &hKey, NULL) != ERROR_SUCCESS)
			return NULL;

		RegCloseKey(hAppKey);
	}

	RegCloseKey(hSoftwareKey);

	return hKey;
}

char *osGetConfigStringKey(char *szName, char *defvalue)
{
	HKEY hKey;
	DWORD dwType, dwLength;
	char *szValue, *szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return strdup(defvalue);

	if (RegQueryValueEx(hKey, szValueName, NULL, &dwType, NULL, &dwLength) != ERROR_SUCCESS)
	{
		RegCloseKey(hKey);
		return strdup(defvalue);
	}

	if (dwType != REG_SZ && dwType != REG_EXPAND_SZ)
	{
		RegCloseKey(hKey);
		return strdup(defvalue);
	}

	szValue = malloc(dwLength);
	if (RegQueryValueEx(hKey, szValueName, NULL, &dwType, szValue, &dwLength) != ERROR_SUCCESS)
	{
		RegCloseKey(hKey);
		free(szValue);
		return strdup(defvalue);
	}

	RegCloseKey(hKey);
	return szValue;
}

void osSetConfigStringKey(char *szName, char *szValue)
{
	HKEY hKey;
	char *szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return;

	RegSetValueEx(hKey, szValueName, 0, REG_SZ, szValue, strlen(szValue)+1);

	RegCloseKey(hKey);
}

int osGetConfigIntKey(char *szName, int defvalue)
{
	HKEY hKey;
	DWORD dwType, dwLength, dwValue;
	char *szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return defvalue;

	dwLength = sizeof(DWORD);
	if (RegQueryValueEx(hKey, szValueName, NULL, &dwType, (LPBYTE) &dwValue, &dwLength) != ERROR_SUCCESS)
	{
		RegCloseKey(hKey);
		return defvalue;
	}

	if (dwType != REG_DWORD)
	{
		RegCloseKey(hKey);
		return defvalue;
	}

	RegCloseKey(hKey);
	return dwValue;
}

void osSetConfigIntKey(char *szName, int nValue)
{
	HKEY hKey;
	char *szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return;

	RegSetValueEx(hKey, szValueName, 0, REG_DWORD, (LPBYTE) &nValue, sizeof(nValue));

	RegCloseKey(hKey);
}

double osGetConfigDoubleKey(char *szName, double defvalue)
{
	HKEY hKey;
	DWORD dwType, dwLength;
	char *szValueName;
	char buffer[64];

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return defvalue;

	dwLength = sizeof(buffer);
	if (RegQueryValueEx(hKey, szValueName, NULL, &dwType, buffer, &dwLength) != ERROR_SUCCESS)
	{
		RegCloseKey(hKey);
		return defvalue;
	}

	if (dwType != REG_SZ && dwType != REG_EXPAND_SZ)
	{
		RegCloseKey(hKey);
		return defvalue;
	}

	RegCloseKey(hKey);
	return atof(buffer);
}

void osSetConfigDoubleKey(char *szName, double dValue)
{
	HKEY hKey;
	char *szValueName;
	char buffer[64];

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return;

	sprintf(buffer, "%f", dValue);
	RegSetValueEx(hKey, szValueName, 0, REG_SZ, buffer, strlen(buffer)+1);

	RegCloseKey(hKey);
}

BOOL osGetConfigBoolKey(char *szName, BOOL defvalue)
{
	HKEY hKey;
	DWORD dwType, dwLength, dwValue;
	char *szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return defvalue;

	dwLength = sizeof(DWORD);
	if (RegQueryValueEx(hKey, szValueName, NULL, &dwType, (LPBYTE) &dwValue, &dwLength) != ERROR_SUCCESS)
	{
		RegCloseKey(hKey);
		return defvalue;
	}

	if (dwType != REG_DWORD)
	{
		RegCloseKey(hKey);
		return defvalue;
	}

	RegCloseKey(hKey);
	return (dwValue != 0);
}

void osSetConfigBoolKey(char *szName, BOOL bValue)
{
	HKEY hKey;
	char *szValueName;
	DWORD dwValue;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return;

	dwValue = bValue ? 1 : 0;
	RegSetValueEx(hKey, szValueName, 0, REG_DWORD, (LPBYTE) &dwValue, sizeof(dwValue));

	RegCloseKey(hKey);
}
