#include "StatusBar.h"
#include "DockBar.h"
#include "Internals.h"
#include "Handlers_stub.h"

void osSetStatusBarVisible(BOOL visible)
{
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	ShowWindow(pData->hStatusBar, visible ? SW_SHOWNORMAL : SW_HIDE);
	RelayoutFrameBars();
}

BOOL osGetStatusBarVisible()
{
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	return IsWindowVisible(pData->hStatusBar);
}

void osPushStatusBarContext(char *title)
{
	StatusContext context;
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	context = (StatusContext) rmalloc(sizeof(struct StatusContext) + strlen(title) + 1);
	context->next = pData->statusContexts;
	strcpy(context->tip, title);
	pData->statusContexts = context;

	SetWindowText(pData->hStatusBar, title);
}

void osPopStatusBarContext()
{
	StatusContext context;
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	context = pData->statusContexts;
	if (!context)
		return;

	pData->statusContexts = context->next;

	SetWindowText(pData->hStatusBar, context->tip);

	free(context);
}

char *osGetStatusBarTitle()
{
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
    int nLen = GetWindowTextLength(pData->hStatusBar);
    char *buffer = (char *) rmalloc(nLen+1);
    GetWindowText(pData->hStatusBar, buffer, nLen+1);
    return buffer;
};

void osSetStatusBarTitle(char *title)
{
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	SetWindowText(pData->hStatusBar, title);
};

int osGetStatusBarIndicatorsCount()
{
	int count;
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	IndicatorHandle indicator;

	count = 0;
	indicator = pData->first_indicator;
	while (indicator)
	{
		indicator = indicator->next;
		count++;
	}

	return count;
};

void RefreshStatusBarIndicators()
{
	int i, x, count, *buffer;
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	IndicatorHandle indicator;
	HDC hDC;
	HFONT hFont;
	RECT rect;

	if (!pData || !pData->hStatusBar)
		return;

	GetClientRect(pData->hStatusBar, &rect);

	// get the actual count of indicators
	count = 0;
	indicator = pData->first_indicator;
	while (indicator)
	{
		indicator = indicator->next;
		count++;
	}

	// allocate the "parts" buffer required from SB_SETPARTS message
	buffer = rmalloc((count+1)*sizeof(int));

	// get the device context and the font for the status bar
	hDC = GetDC(pData->hStatusBar);
	hFont = (HFONT) SendMessage(pData->hStatusBar,WM_GETFONT,0,0);
	if (hFont)
		SelectObject(hDC, hFont);

	// populate the "parts" buffer
	i = count+1;
	x = rect.right-rect.left-GetSystemMetrics(SM_CXVSCROLL);
	indicator = pData->last_indicator;
	while (indicator)
	{
		SIZE sz;

		buffer[--i] = x;

		sz.cx = 0;
		sz.cy = 0;
		if (indicator->title)
			GetTextExtentPoint32(hDC, indicator->title, strlen(indicator->title), &sz);
		x -= sz.cx+5;

		indicator = indicator->prev;
	}
	buffer[0] = x;

	// set parts
	SendMessage(pData->hStatusBar, SB_SETPARTS, count+1, (LPARAM) buffer);

	// refresh the part titles
	i = 1;
	indicator = pData->first_indicator;
	while (indicator)
	{
		if (indicator->title)
			SendMessage(pData->hStatusBar, SB_SETTEXT, i, (LPARAM) indicator->title);

		indicator = indicator->next;
		i++;
	}

	// release the device context
	ReleaseDC(pData->hStatusBar, hDC);

	// free parts buffer
	free(buffer);
}

IndicatorHandle osCreateIndicator(int index)
{
	IndicatorHandle indicator, *link, last;
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	indicator = rmalloc(sizeof(struct IndicatorHandle));
	indicator->title = NULL;

	if (index >= 0)
	{
		last = NULL;
		link = &pData->first_indicator;

		while (index > 0 && *link != NULL)
		{
			last = *link;
			link = &last->next;
			index--;
		}
	}
	else
	{
		if (pData->last_indicator)
		{
			last = pData->last_indicator;
			link = &pData->last_indicator->next;
		}
		else
		{
			last = NULL;
			link = &pData->first_indicator;
		}
	}

	indicator->next = *link;
	indicator->prev = last;
	if (*link) (*link)->prev = indicator;
	*link = indicator;

	if (last == pData->last_indicator)
		pData->last_indicator = indicator;

	RefreshStatusBarIndicators();

	return indicator;
}

void osDestroyIndicator(IndicatorHandle indicator)
{
	FrameData *pData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	handleIndicatorDestroy(indicator);

	if (indicator->next) indicator->next->prev = indicator->prev;
	if (indicator->prev) indicator->prev->next = indicator->next;

	if (indicator == pData->first_indicator)
		pData->first_indicator = indicator->next;
	if (indicator == pData->last_indicator)
		pData->last_indicator  = indicator->prev;

	free(indicator->title);
	free(indicator);

	RefreshStatusBarIndicators();
}

char *osGetIndicatorTitle(IndicatorHandle indicator)
{
	return strdup(indicator->title);
}

void osSetIndicatorTitle(IndicatorHandle indicator, char *title)
{
	if (indicator->title)
		free(indicator->title);
	indicator->title = strdup(title);

	RefreshStatusBarIndicators();
}

int osGetIndicatorPos(IndicatorHandle indicator)
{
	int pos;

	pos = 0;
	while ((indicator = indicator->prev) != NULL)
		pos++;

	return pos;
}
