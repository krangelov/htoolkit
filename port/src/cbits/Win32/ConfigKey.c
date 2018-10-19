#include "ConfigKey.h"
#include "Internals.h"
#include "Types.h"

static HKEY openKey(PortString szName, PortString *pszValueName)
{
	PortString s;
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

	if (RegOpenKeyExW(HKEY_CURRENT_USER, L"Software", 0, KEY_READ, &hSoftwareKey) != ERROR_SUCCESS)
		return NULL;

	if (RegCreateKeyExW(hSoftwareKey, pFrameData->lpszAppName, 0, 0, REG_OPTION_NON_VOLATILE, KEY_READ | KEY_WRITE, NULL, &hAppKey, NULL)!= ERROR_SUCCESS)
		return NULL;

	if (*pszValueName == szName)
		return hAppKey;
	else
	{
		(*pszValueName)[-1] = 0;
		if (RegCreateKeyExW(hAppKey, szName, 0, 0, REG_OPTION_NON_VOLATILE, KEY_READ | KEY_WRITE, NULL, &hKey, NULL) != ERROR_SUCCESS)
			return NULL;

		RegCloseKey(hAppKey);
	}

	RegCloseKey(hSoftwareKey);

	return hKey;
}

PortString osGetConfigStringKey(PortString szName, PortString defvalue)
{
	HKEY hKey;
	DWORD dwType, dwLength;
	PortString szValue, szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return psdup(defvalue);

	if (RegQueryValueExW(hKey, szValueName, NULL, &dwType, NULL, &dwLength) != ERROR_SUCCESS)
	{
		RegCloseKey(hKey);
		return psdup(defvalue);
	}

	if (dwType != REG_SZ && dwType != REG_EXPAND_SZ)
	{
		RegCloseKey(hKey);
		return psdup(defvalue);
	}

	szValue = malloc(sizeof(wchar_t)*(dwLength+1));
	if (RegQueryValueExW(hKey, szValueName, NULL, &dwType, (LPBYTE) szValue, &dwLength) != ERROR_SUCCESS)
	{
		RegCloseKey(hKey);
		free(szValue);
		return psdup(defvalue);
	}

	RegCloseKey(hKey);
	return szValue;
}

void osSetConfigStringKey(PortString szName, PortString szValue)
{
	HKEY hKey;
	PortString szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return;

	RegSetValueExW(hKey, szValueName, 0, REG_SZ, (LPBYTE) szValue, pslen(szValue)+1);

	RegCloseKey(hKey);
}

int osGetConfigIntKey(PortString szName, int defvalue)
{
	HKEY hKey;
	DWORD dwType, dwLength, dwValue;
	PortString szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return defvalue;

	dwLength = sizeof(DWORD);
	if (RegQueryValueExW(hKey, szValueName, NULL, &dwType, (LPBYTE) &dwValue, &dwLength) != ERROR_SUCCESS)
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

void osSetConfigIntKey(PortString szName, int nValue)
{
	HKEY hKey;
	PortString szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return;

	RegSetValueExW(hKey, szValueName, 0, REG_DWORD, (LPBYTE) &nValue, sizeof(nValue));

	RegCloseKey(hKey);
}

double osGetConfigDoubleKey(PortString szName, double defvalue)
{
	HKEY hKey;
	DWORD dwType, dwLength;
	PortString szValueName;
	char buffer[64];

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return defvalue;

	dwLength = sizeof(buffer);
	if (RegQueryValueExW(hKey, szValueName, NULL, &dwType, buffer, &dwLength) != ERROR_SUCCESS)
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

void osSetConfigDoubleKey(PortString szName, double dValue)
{
	HKEY hKey;
	PortString szValueName;
	WCHAR buffer[64];

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return;

	ftops(dValue, buffer);
	RegSetValueExW(hKey, szValueName, 0, REG_SZ, (LPBYTE) buffer, pslen(buffer)+1);

	RegCloseKey(hKey);
}

BOOL osGetConfigBoolKey(PortString szName, BOOL defvalue)
{
	HKEY hKey;
	DWORD dwType, dwLength, dwValue;
	PortString szValueName;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return defvalue;

	dwLength = sizeof(DWORD);
	if (RegQueryValueExW(hKey, szValueName, NULL, &dwType, (LPBYTE) &dwValue, &dwLength) != ERROR_SUCCESS)
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

void osSetConfigBoolKey(PortString szName, BOOL bValue)
{
	HKEY hKey;
	PortString szValueName;
	DWORD dwValue;

	hKey = openKey(szName, &szValueName);
	if (!hKey)
		return;

	dwValue = bValue ? 1 : 0;
	RegSetValueExW(hKey, szValueName, 0, REG_DWORD, (LPBYTE) &dwValue, sizeof(dwValue));

	RegCloseKey(hKey);
}
