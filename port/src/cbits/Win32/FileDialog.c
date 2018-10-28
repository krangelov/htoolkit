#include "CommonDialogs.h"
#include "Internals.h"

static UINT_PTR APIENTRY FileSelectorHook (HWND hdlg, UINT uiMsg, WPARAM wParam, LPARAM lParam)
{
	if (uiMsg == WM_INITDIALOG)
	{
		RECT	rect;
		int		x, y;
		GetWindowRect (hdlg, &rect);

		x	= (GetSystemMetrics (SM_CXSCREEN)>>1) - ((rect.right-rect.left)>>1);
		y	= (GetSystemMetrics (SM_CYSCREEN)>>1) - ((rect.bottom-rect.top)>>1);
		SetWindowPos (hdlg, NULL, x, y, 0, 0, SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOZORDER);
	}

	return 0;
}

char *osSelectDirectory(char *title, WindowHandle owner)
{
	char buffer[MAX_PATH];
	LPITEMIDLIST pidlReturn;
	BROWSEINFO bi;
	char *s = NULL;

	bi.hwndOwner      = owner ? owner : ghWndFrame;
	bi.pidlRoot       = NULL;
	bi.pszDisplayName = buffer;
	bi.lpszTitle      = title;
	bi.ulFlags        = BIF_RETURNONLYFSDIRS;
	bi.lpfn           = NULL;
	bi.lParam         = 0;

	pidlReturn = SHBrowseForFolder (&bi);
	if (pidlReturn)
	{
		s = (char *) rmalloc (MAX_PATH+1);
		SHGetPathFromIDList (pidlReturn,s);
		CoTaskMemFree (pidlReturn);
	}

	return s;
}

char *osSelectInputFile(char *title, char *filter, WindowHandle owner)
{
	OPENFILENAME ofn;

	ofn.lStructSize       = sizeof (OPENFILENAME);
	ofn.hwndOwner         = owner ? owner : ghWndFrame;
	ofn.hInstance         = NULL;
	ofn.lpstrFilter       = filter;
	ofn.lpstrCustomFilter = NULL;
	ofn.nMaxCustFilter    = 0;
	ofn.nFilterIndex      = 0;
	ofn.lpstrFile         = (LPSTR) rmalloc (MAX_PATH);
	ofn.lpstrFile[0]      = '\0';
	ofn.nMaxFile          = MAX_PATH;
	ofn.lpstrFileTitle    = NULL;
	ofn.nMaxFileTitle     = 0;
	ofn.lpstrInitialDir   = NULL;
	ofn.lpstrTitle        = title;
	ofn.Flags             = OFN_EXPLORER
						  | OFN_FILEMUSTEXIST
						  |	OFN_HIDEREADONLY
						  | OFN_PATHMUSTEXIST
						  | OFN_ENABLEHOOK;
	ofn.lpstrDefExt       = NULL;
	ofn.lCustData         = 0;
	ofn.lpfnHook          = &FileSelectorHook;
	ofn.lpTemplateName    = NULL;

	if (GetOpenFileName(&ofn))
		return ofn.lpstrFile;

	rfree (ofn.lpstrFile);
	return NULL;
}

char *osSelectInputFiles(char *title, char *filter, WindowHandle owner)
{
	char *buffer;
	OPENFILENAME ofn;

	buffer = malloc(MAX_PATH*2);
	if (!buffer)
		return NULL;

	ofn.lStructSize       = sizeof (OPENFILENAME);
	ofn.hwndOwner         = owner ? owner : ghWndFrame;
	ofn.hInstance         = NULL;
	ofn.lpstrFilter       = filter;
	ofn.lpstrCustomFilter = NULL;
	ofn.nMaxCustFilter    = 0;
	ofn.nFilterIndex      = 0;
	ofn.lpstrFile         = buffer;
	ofn.lpstrFile[0]      = '\0';
	ofn.nMaxFile          = MAX_PATH*2;
	ofn.lpstrFileTitle    = NULL;
	ofn.nMaxFileTitle     = 0;
	ofn.lpstrInitialDir   = NULL;
	ofn.lpstrTitle        = title;
	ofn.Flags             = OFN_EXPLORER
						  | OFN_FILEMUSTEXIST
						  |	OFN_HIDEREADONLY
						  | OFN_PATHMUSTEXIST
						  | OFN_ENABLEHOOK
						  | OFN_ALLOWMULTISELECT;
	ofn.lpstrDefExt       = NULL;
	ofn.lCustData         = 0;
	ofn.lpfnHook          = &FileSelectorHook;
	ofn.lpTemplateName    = NULL;

	if (GetOpenFileName(&ofn))
	{
		int nLen, nSize;
		char *s, *s2, *buffer;

		nSize = 1;
		s = ofn.lpstrFile+ofn.nFileOffset;
		while (*s)
		{
			nLen = strlen(s);
			s += nLen+1;
			nSize += ofn.nFileOffset+nLen+1;
		}

		buffer = malloc(nSize);
		if (!buffer)
		{
			free(ofn.lpstrFile);
			return NULL;
		}

		s2 = buffer;
		s  = ofn.lpstrFile+ofn.nFileOffset;
		while (*s)
		{
			memcpy(s2,ofn.lpstrFile,ofn.nFileOffset-1);
			s2[ofn.nFileOffset-1] = '\\';
			strcpy(s2+ofn.nFileOffset,s);
			nLen = strlen(s);
			s  += nLen+1;
			s2 += ofn.nFileOffset+nLen+1;
		}
		*s2 = 0;

		free(ofn.lpstrFile);
		return buffer;
	}

	free(ofn.lpstrFile);
	return NULL;
}

char *osSelectOutputFile(char *title, char *filter, char *nameptr, WindowHandle owner)
{
	OPENFILENAME ofn;

	if (strlen(title) == 0)
		title = NULL;

	ofn.lStructSize       = sizeof (OPENFILENAME);
	ofn.hwndOwner         = owner ? owner : ghWndFrame;
	ofn.lpstrFilter       = filter;
	ofn.lpstrCustomFilter = NULL;
	ofn.nMaxCustFilter    = 0;
	ofn.nFilterIndex      = 0;
	ofn.lpstrFile         = (LPSTR) rmalloc (MAX_PATH);

	strncpy(ofn.lpstrFile, nameptr, MAX_PATH - 1);
	ofn.lpstrFile[MAX_PATH - 1] = '\0';

	ofn.nMaxFile        = MAX_PATH;
	ofn.lpstrFileTitle  = NULL;
	ofn.nMaxFileTitle   = 0;
	ofn.lpstrInitialDir = NULL;
	ofn.lpstrTitle      = title;
	ofn.Flags           = OFN_EXPLORER
						| OFN_OVERWRITEPROMPT
						| OFN_HIDEREADONLY
						| OFN_ENABLEHOOK;
	ofn.lpstrDefExt     = NULL;
	ofn.lCustData       = 0;
	ofn.lpfnHook        = &FileSelectorHook;
	ofn.lpTemplateName  = NULL;

	if (GetSaveFileName (&ofn))
		return ofn.lpstrFile;

	rfree (ofn.lpstrFile);
	return NULL;
}
