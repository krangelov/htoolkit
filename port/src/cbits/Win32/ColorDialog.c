#include "CommonDialogs.h"
#include "Internals.h"

static COLORREF std_colors[] =
{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

BOOL osRunColorDialog(unsigned int *color, WindowHandle owner)
{
	CHOOSECOLOR cc;

	cc.lStructSize = sizeof(cc);
	cc.hwndOwner = owner ? owner : ghWndFrame;
	cc.hInstance = NULL;
	cc.rgbResult = 0;
	cc.lpCustColors = std_colors;
	cc.Flags = 0;
	cc.lCustData = 0;
	cc.lpfnHook = NULL;
	cc.lpTemplateName = NULL;

	if (!ChooseColor(&cc))
		return FALSE;

	*color = cc.rgbResult;
	return TRUE;
}
