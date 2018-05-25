#include "CommonDialogs.h"
#include "Internals.h"

BOOL osRunFontDialog(char **fname, int *fsize, int *fweight, int *fstyle, BOOL *funderline, BOOL *fstrikeout, WindowHandle owner)
{
	CHOOSEFONT cf;
	LOGFONT lf;

	cf.lStructSize = sizeof(cf);
	cf.hwndOwner = owner ? owner : ghWndFrame;
	cf.hDC = NULL;
	cf.lpLogFont = &lf;
	cf.iPointSize = 0;
	cf.Flags = CF_SCREENFONTS;
	cf.rgbColors = RGB(0,0,0);
	cf.lCustData = 0;
	cf.lpfnHook = NULL;
	cf.lpTemplateName = NULL;
	cf.hInstance = NULL;
	cf.lpszStyle = NULL;
	cf.nFontType = 0;
	cf.nSizeMin = 0;
	cf.nSizeMax = 0;

	if (!ChooseFont(&cf))
		return FALSE;

	*fname = strdup(lf.lfFaceName);
	*fsize = -lf.lfHeight;
	*fweight = lf.lfWeight;
	*fstyle = lf.lfItalic ? 1 : 0;
	*funderline = lf.lfUnderline;
	*fstrikeout = lf.lfStrikeOut;

	return TRUE;
}
