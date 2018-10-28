#include "Message.h"
#include "Internals.h"

void osMessageAlert(PortString szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	MessageBoxW(ghWndFrame, szText, pFrameData->lpszAppName, MB_OK | MB_ICONINFORMATION);
};

BOOL osMessageConfirm(PortString szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	return (MessageBoxW(ghWndFrame, szText, pFrameData->lpszAppName, MB_OKCANCEL | MB_ICONINFORMATION) == IDOK);
};

void osMessageWarning(PortString szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	MessageBoxW(ghWndFrame, szText, pFrameData->lpszAppName, MB_OK | MB_ICONWARNING);
};

BOOL osMessageQuestion(PortString szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	return (MessageBoxW(ghWndFrame, szText, pFrameData->lpszAppName, MB_YESNO | MB_ICONQUESTION) == IDYES);
};

BOOL osMessageError(PortString szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	return (MessageBoxW(ghWndFrame, szText, pFrameData->lpszAppName, MB_OKCANCEL | MB_ICONERROR) == IDOK);
};

int osMessageCancelQuestion(PortString szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	switch (MessageBoxW(ghWndFrame, szText, pFrameData->lpszAppName, MB_YESNO | MB_ICONQUESTION))
	{
		case IDNO:  return 0;
		case IDYES: return 1;
		default:    return -1;
	}
};

int osMessageConfirmSave(PortString szText)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	switch (MessageBoxW(ghWndFrame, szText, pFrameData->lpszAppName, MB_YESNO | MB_ICONQUESTION))
	{
		case IDNO:  return 0;
		case IDYES: return 1;
		default:    return -1;
	}
};
