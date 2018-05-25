#include "Message.h"
#include "Internals.h"

void osMessageAlert(char *szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	MessageBox(ghWndFrame, szText, pFrameData->lpszAppName, MB_OK | MB_ICONINFORMATION);
};

BOOL osMessageConfirm(char *szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	return (MessageBox(ghWndFrame, szText, pFrameData->lpszAppName, MB_OKCANCEL | MB_ICONINFORMATION) == IDOK);
};

void osMessageWarning(char *szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	MessageBox(ghWndFrame, szText, pFrameData->lpszAppName, MB_OK | MB_ICONWARNING);
};

BOOL osMessageQuestion(char *szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	return (MessageBox(ghWndFrame, szText, pFrameData->lpszAppName, MB_YESNO | MB_ICONQUESTION) == IDYES);
};

BOOL osMessageError(char *szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	return (MessageBox(ghWndFrame, szText, pFrameData->lpszAppName, MB_OKCANCEL | MB_ICONERROR) == IDOK);
};

int osMessageCancelQuestion(char *szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	switch (MessageBox(ghWndFrame, szText, pFrameData->lpszAppName, MB_YESNO | MB_ICONQUESTION))
	{
		case IDNO:  return 0;
		case IDYES: return 1;
		default:    return -1;
	}
};

int osMessageConfirmSave(char *szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	switch (MessageBox(ghWndFrame, szText, pFrameData->lpszAppName, MB_YESNO | MB_ICONQUESTION))
	{
		case IDNO:  return 0;
		case IDYES: return 1;
		default:    return -1;
	}
};
