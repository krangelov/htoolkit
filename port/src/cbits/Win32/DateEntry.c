#include "DateEntry.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateDateEntry(WindowHandle window)
{
	HWND hEntry;
	
	hEntry = CreateWindowEx(
			  WS_EX_CLIENTEDGE,
			  DATETIMEPICK_CLASS,
			  NULL,
			  WS_CHILD | WS_BORDER | WS_TABSTOP,
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);
	
	return checkWindow(hEntry, DATETIMEPICK_CLASS);
};

void osGetDateEntryReqSize(WindowHandle entry, int *res)
{
	SIZE sz;
	HDC hDC = GetDC(entry);
	HFONT hFont = (HFONT) SendMessage(entry,WM_GETFONT,0,0);
	char buffer[20];

	GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SSHORTDATE, buffer, 20);
	
	if (hFont) SelectObject(hDC, hFont);
	GetTextExtentPoint32(hDC, buffer, strlen(buffer), &sz);
	ReleaseDC(entry, hDC);
	
	res[0] = sz.cx + GetSystemMetrics(SM_CXBORDER)*2 + GetSystemMetrics(SM_CXVSCROLL);
	res[1] = sz.cy + GetSystemMetrics(SM_CYBORDER)*2 + 6;
}

time_t osGetDateEntryValue(WindowHandle entry)
{
	struct tm tm;
	SYSTEMTIME stm;
	SendMessage(entry, DTM_GETSYSTEMTIME, 0, (LPARAM) &stm);

	tm.tm_sec  = stm.wSecond;
	tm.tm_min  = stm.wMinute;
	tm.tm_hour = stm.wHour;
	tm.tm_mday = stm.wDay;
	tm.tm_mon  = stm.wMonth-1;
	tm.tm_year = stm.wYear-1900;
	tm.tm_wday = stm.wDayOfWeek;
	tm.tm_yday = 0;
	tm.tm_isdst = -1;
	return mktime(&tm);
}

void osSetDateEntryValue(WindowHandle entry, time_t value)
{
	struct tm *ptm;
	SYSTEMTIME stm;
	
	ptm = localtime(&value);
	stm.wSecond    = ptm->tm_sec;
	stm.wMinute    = ptm->tm_min;
	stm.wHour      = ptm->tm_hour;
	stm.wDay       = ptm->tm_mday;
	stm.wMonth     = ptm->tm_mon+1;
	stm.wYear      = ptm->tm_year+1900;
	stm.wDayOfWeek = ptm->tm_wday;
	
	SendMessage(entry, DTM_SETSYSTEMTIME, 0, (LPARAM) &stm);
}
