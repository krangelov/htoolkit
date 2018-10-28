#include "CommonDialogs.h"
#include "Internals.h"

#define FONT_SIZE 15
#define FONT_FACE "MS Sans Serif"

#define IDC_CREDITS 999

typedef struct
{
	HDC hDC;
	HFONT hLargeFont, hFont;
	int nBaseUnitX, nBaseUnitY;
	BitmapHandle bitmap;
	char *authors;
	char *documenters;
	char *translator_credits;
	
	// Dialog template buffer
	WORD *pBuffer;
	WORD *pBufferPos;
	UINT nWordsAllocated;	
} DlgInfo;

static BOOL InitDlgInfo(DlgInfo *pdi, BitmapHandle bitmap, char *authors, char *documenters, char *translator_credits)
{
	LOGFONT lf;
	
	memset(pdi, 0, sizeof(*pdi));
	
	pdi->bitmap = bitmap;
	pdi->authors = authors;
	pdi->documenters = documenters;
	pdi->translator_credits = translator_credits;
	
	pdi->hDC = CreateDC ("DISPLAY", NULL, NULL, NULL);
	if (!pdi->hDC) return FALSE;

	lf.lfHeight         = -FONT_SIZE*2;
	lf.lfWeight         = FW_BOLD;
	lf.lfItalic         = FALSE;
	lf.lfUnderline      = FALSE;
	lf.lfStrikeOut      = FALSE;
	lf.lfWidth          = 0;
	lf.lfEscapement     = 0;
	lf.lfOrientation    = 0;
	lf.lfCharSet        = DEFAULT_CHARSET;
	lf.lfOutPrecision   = OUT_DEFAULT_PRECIS;
	lf.lfClipPrecision  = CLIP_DEFAULT_PRECIS;
	lf.lfQuality        = DEFAULT_QUALITY;
	lf.lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
	strcpy(lf.lfFaceName, FONT_FACE);
			
	pdi->hLargeFont = CreateFontIndirect(&lf);
	if (!pdi->hLargeFont) return FALSE;
	
	lf.lfHeight         = -FONT_SIZE;
	lf.lfWeight         = FW_NORMAL;
	lf.lfItalic         = FALSE;
	lf.lfUnderline      = FALSE;
	lf.lfStrikeOut      = FALSE;
	lf.lfWidth          = 0;
	lf.lfEscapement     = 0;
	lf.lfOrientation    = 0;
	lf.lfCharSet        = DEFAULT_CHARSET;
	lf.lfOutPrecision   = OUT_DEFAULT_PRECIS;
	lf.lfClipPrecision  = CLIP_DEFAULT_PRECIS;
	lf.lfQuality        = DEFAULT_QUALITY;
	lf.lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
	strcpy(lf.lfFaceName, FONT_FACE);

	pdi->hFont = CreateFontIndirect(&lf);
	if (!pdi->hFont) return FALSE;
	
	pdi->nBaseUnitX = LOWORD(GetDialogBaseUnits());
	pdi->nBaseUnitY = HIWORD(GetDialogBaseUnits());
	
	pdi->pBuffer         = NULL;
	pdi->pBufferPos      = NULL;
	pdi->nWordsAllocated = 0;
		
	return TRUE;
}

static BOOL FreeDlgInfo(DlgInfo *pdi)
{
	if (pdi->hFont)      DeleteObject(pdi->hFont);
	if (pdi->hLargeFont) DeleteObject(pdi->hLargeFont);
	if (pdi->hDC)        DeleteDC(pdi->hDC);
	if (pdi->pBuffer)    free(pdi->pBuffer);
	return TRUE;
}

static void ResetBufferPos(DlgInfo *pdi)
{
	pdi->pBufferPos = pdi->pBuffer;
}

static WORD *EnsureAtLeastNWords(DlgInfo *pdi, UINT nCount)
{
	WORD *pNewBuffer;
	UINT nWordsFree;

	nWordsFree = pdi->nWordsAllocated-(pdi->pBufferPos-pdi->pBuffer);
	
	if (nWordsFree < nCount)
	{	
		 pdi->nWordsAllocated += max(nCount-nWordsFree,128);
		 pNewBuffer = realloc(pdi->pBuffer, pdi->nWordsAllocated*sizeof(WORD));
		 pdi->pBufferPos += pNewBuffer - pdi->pBuffer;
		 pdi->pBuffer = pNewBuffer;
	}
	
	return pdi->pBufferPos;
}

static void StoreWord(DlgInfo *pdi, WORD w)
{
	EnsureAtLeastNWords(pdi, 1);
	*(pdi->pBufferPos)++ = w;
}

static void StoreDWord(DlgInfo *pdi, DWORD dw)
{
	EnsureAtLeastNWords(pdi, 1);
	*(pdi->pBufferPos)++ = LOWORD(dw);
	*(pdi->pBufferPos)++ = HIWORD(dw);
}

static void StoreString(DlgInfo *pdi, char *s)
{
	EnsureAtLeastNWords(pdi, strlen(s)+1);
	do *(pdi->pBufferPos)++ = (WORD) *s; while (*s++);
}

static void StoreStringZ(DlgInfo *pdi, char *s)
{
	EnsureAtLeastNWords(pdi, strlen(s));
	while (*s) *(pdi->pBufferPos)++ = (WORD) *s++;
}

static void StoreRect(DlgInfo *pdi, UINT x, UINT y, UINT cx, UINT cy)
{
	EnsureAtLeastNWords(pdi, 4);
	*(pdi->pBufferPos)++ = x;
	*(pdi->pBufferPos)++ = y;
	*(pdi->pBufferPos)++ = cx;
	*(pdi->pBufferPos)++ = cy;
}

static void AlignBufferPos(DlgInfo *pdi)
{
	while ((pdi->pBufferPos-pdi->pBuffer) % (sizeof(DWORD)/sizeof(WORD)))
		StoreWord(pdi, 0);
}

static void GetBmpSizeInDlgUnit(DlgInfo *pdi, BitmapHandle bitmap, SIZE *psz)
{
	psz->cx = MulDiv(bitmap->destsize.cx,4,pdi->nBaseUnitX);
	psz->cy = MulDiv(bitmap->destsize.cy,8,pdi->nBaseUnitY);
}

static void GetTextSizeInDlgUnit(DlgInfo *pdi, char *s, SIZE *psz, BOOL bLarge)
{
	RECT rect;
	HFONT hOldFont;
	memset(&rect, 0, sizeof(rect));
	
	hOldFont = SelectObject(pdi->hDC, (bLarge) ? pdi->hLargeFont : pdi->hFont);
	DrawText(pdi->hDC, s, strlen(s), &rect, DT_CALCRECT);
	SelectObject(pdi->hDC, hOldFont);

	psz->cx = MulDiv(rect.right-rect.left,4,pdi->nBaseUnitX);
	psz->cy = MulDiv(rect.bottom-rect.top,8,pdi->nBaseUnitY);
}


static INT_PTR CALLBACK HDetailsDialogFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_INITDIALOG:
		SendDlgItemMessage(hWnd, 100, WM_SETFONT, (WPARAM) ((DlgInfo *) ((PROPSHEETPAGE *) lParam)->lParam)->hFont, TRUE);
		return TRUE;
	}
	
	return FALSE;
}

static INT_PTR CALLBACK HAboutDialogFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	DlgInfo *pdi;
	
	switch (uMsg)
	{
	case WM_INITDIALOG:
		{
			BitmapHandle bitmap;
			
			pdi = (DlgInfo *) lParam;
			bitmap = pdi->bitmap;
			
			SetWindowLongPtrW(hWnd, DWLP_USER, lParam);
	
			if (bitmap->destsize.cx != bitmap->sourcesize.cx || bitmap->destsize.cy != bitmap->sourcesize.cy)
			{
				HBITMAP hBitmap;
				HDC hDC, hDestDC, hSourceDC;
		
				hDC = GetDC(hWnd);
				hDestDC   = CreateCompatibleDC(hDC);
				hSourceDC = CreateCompatibleDC(hDC);
			
				hBitmap = CreateCompatibleBitmap(hDC, bitmap->destsize.cx, bitmap->destsize.cy);
				SelectObject(hDestDC, hBitmap);
			
				SelectObject(hSourceDC, bitmap->hBitmap);
	
				StretchBlt(hDestDC, 0, 0, bitmap->destsize.cx, bitmap->destsize.cy, hSourceDC, 0, 0, bitmap->sourcesize.cx, bitmap->sourcesize.cy, SRCCOPY);
	
				DeleteObject(bitmap->hBitmap);
				bitmap->hBitmap = hBitmap;
				bitmap->sourcesize = bitmap->destsize;
			
				DeleteDC(hSourceDC);
				DeleteDC(hDestDC);
			
				ReleaseDC(hWnd,hDC);
			}

			SendDlgItemMessage(hWnd, 100, STM_SETIMAGE, IMAGE_BITMAP, (LPARAM) bitmap->hBitmap);
			SendDlgItemMessage(hWnd, 101, WM_SETFONT, (WPARAM) pdi->hLargeFont, TRUE);
			SendDlgItemMessage(hWnd, 102, WM_SETFONT, (WPARAM) pdi->hFont, TRUE);
			SendDlgItemMessage(hWnd, 103, WM_SETFONT, (WPARAM) pdi->hFont, TRUE);
			SendDlgItemMessage(hWnd, IDC_CREDITS, WM_SETFONT, (WPARAM) pdi->hFont, TRUE);
			SendDlgItemMessage(hWnd, 104, WM_SETFONT, (WPARAM) pdi->hFont, TRUE);			
			SendDlgItemMessage(hWnd, IDOK,WM_SETFONT, (WPARAM) pdi->hFont, TRUE);
		}
		return TRUE;
	case WM_CLOSE:
		EndDialog(hWnd,0);
		return TRUE;
	case WM_COMMAND:
		switch (LOWORD(wParam))
		{
		case IDOK:
			EndDialog(hWnd,0);
			return TRUE;
		case IDC_CREDITS:
			{
				SIZE sz;
				PROPSHEETHEADER psh;
				PROPSHEETPAGE psp[3];
				int nPageIndex, n;
				char *s;
				
				pdi = (DlgInfo *) GetWindowLongPtrW(hWnd, DWLP_USER);
				
				nPageIndex = 0;
				memset(&psp, 0, sizeof(psp));
				ResetBufferPos(pdi);
					
				if (pdi->authors)
				{
					psp[nPageIndex].dwSize = sizeof(PROPSHEETPAGE);
					psp[nPageIndex].dwFlags= PSP_DLGINDIRECT;
					psp[nPageIndex].hInstance = ghModule;
					psp[nPageIndex].pResource  = (LPCDLGTEMPLATE) pdi->pBufferPos;
					psp[nPageIndex].pfnDlgProc  = HDetailsDialogFunction;
					psp[nPageIndex].lParam = (LPARAM) pdi;
					nPageIndex++;

					n = 0;
					s = pdi->authors;
					for (;;)
					{ 
						if (*s) n++;
						else
						{
							if (n == 0)
								break;
							n = 0;
							*s = '\n';
						}
						
						s++;
					}
					
					GetTextSizeInDlgUnit(pdi,pdi->authors,&sz,FALSE);
					
					// start to fill in the dlgtemplate information. Addressing by WORDs
					StoreDWord(pdi, 0);                                      // lStyle
					StoreDWord(pdi, 0);                                      // lExtendedStyle
					StoreWord(pdi, 1);                                       // NumberOfItems
					StoreRect(pdi, 0,0,
					               MulDiv(sz.cx,4,3)+4,MulDiv(sz.cy,4,3)+4); // rect
					StoreWord(pdi, 0);                                       // Menu
					StoreWord(pdi, 0);   	                                 // Default dialog class
					StoreString(pdi, "Written by");                          // Title
									
					// start fill in the control templates	
					AlignBufferPos(pdi);

					// comments
					StoreDWord(pdi, WS_VISIBLE | WS_CHILD | SS_NOPREFIX | SS_LEFT);  // lStyle
					StoreDWord(pdi, 0);                                      // lExtendedStyle
					StoreRect(pdi, 2,2,sz.cx,sz.cy);                         // rect
					StoreWord(pdi, 100);                                     // control id
					StoreDWord(pdi, 0x0082FFFF);                             // control class 'STATIC'
					StoreString(pdi, pdi->authors);
					StoreWord(pdi, 0);
				}

				if (pdi->documenters)
				{
					psp[nPageIndex].dwSize = sizeof(PROPSHEETPAGE);
					psp[nPageIndex].dwFlags= PSP_DLGINDIRECT;
					psp[nPageIndex].hInstance = ghModule;
					psp[nPageIndex].pResource  = (LPCDLGTEMPLATE) pdi->pBufferPos;
					psp[nPageIndex].pfnDlgProc  = HDetailsDialogFunction;
					psp[nPageIndex].lParam = (LPARAM) pdi;
					nPageIndex++;

					n = 0;
					s = pdi->authors;
					for (;;)
					{ 
						if (*s) n++;
						else
						{
							if (n == 0)
								break;
							n = 0;
							*s = (WORD) '\n';
						}

						s++;
					}

					GetTextSizeInDlgUnit(pdi,pdi->authors,&sz,FALSE);

					// start to fill in the dlgtemplate information. Addressing by WORDs
					StoreDWord(pdi, 0);                                      // lStyle
					StoreDWord(pdi, 0);                                      // lExtendedStyle
					StoreWord(pdi, 1);                                       // NumberOfItems
					StoreRect(pdi, 0,0,
								   MulDiv(sz.cx,4,3)+4,MulDiv(sz.cy,4,3)+4); // rect
					StoreWord(pdi, 0);                                       // Menu
					StoreWord(pdi, 0);   	                                 // Default dialog class
					StoreString(pdi, "Documented by");                       // Title

					// start fill in the control templates	
					AlignBufferPos(pdi);

					// comments
					StoreDWord(pdi, WS_VISIBLE | WS_CHILD | SS_NOPREFIX | SS_LEFT);  // lStyle
					StoreDWord(pdi, 0);                                      // lExtendedStyle
					StoreRect(pdi, 2,2,sz.cx,sz.cy);                         // rect
					StoreWord(pdi, 100);                                     // control id
					StoreDWord(pdi, 0x0082FFFF);                             // control class 'STATIC'
					StoreString(pdi, pdi->documenters);
					StoreWord(pdi, 0);
				}

				if (pdi->translator_credits)
				{
					psp[nPageIndex].dwSize = sizeof(PROPSHEETPAGE);
					psp[nPageIndex].dwFlags= PSP_DLGINDIRECT;
					psp[nPageIndex].hInstance = ghModule;
					psp[nPageIndex].pResource  = (LPCDLGTEMPLATE) pdi->pBufferPos;
					psp[nPageIndex].pfnDlgProc  = HDetailsDialogFunction;
					psp[nPageIndex].lParam = (LPARAM) pdi;
					nPageIndex++;

					GetTextSizeInDlgUnit(pdi,pdi->authors,&sz,FALSE);

					// start to fill in the dlgtemplate information. Addressing by WORDs
					StoreDWord(pdi, 0);                                      // lStyle
					StoreDWord(pdi, 0);                                      // lExtendedStyle
					StoreWord(pdi, 1);                                       // NumberOfItems
					StoreRect(pdi, 0,0,
								   MulDiv(sz.cx,4,3)+4,MulDiv(sz.cy,4,3)+4); // rect
					StoreWord(pdi, 0);                                       // Menu
					StoreWord(pdi, 0);   	                                 // Default dialog class
					StoreString(pdi, "Translated by");                       // Title

					// start fill in the control templates	
					AlignBufferPos(pdi);

					// comments
					StoreDWord(pdi, WS_VISIBLE | WS_CHILD | SS_NOPREFIX | SS_LEFT);  // lStyle
					StoreDWord(pdi, 0);                                      // lExtendedStyle
					StoreRect(pdi, 2,2,sz.cx,sz.cy);                         // rect
					StoreWord(pdi, 100);                                     // control id
					StoreDWord(pdi, 0x0082FFFF);                             // control class 'STATIC'
					StoreString(pdi, pdi->translator_credits);
					StoreWord(pdi, 0);
				}

				memset(&psh, 0, sizeof(psh));
				psh.dwSize = sizeof(psh);
				psh.dwFlags = PSH_PROPSHEETPAGE | PSH_NOAPPLYNOW;
				psh.hwndParent = hWnd;
				psh.hInstance = ghModule;
				psh.pszCaption = "Details";
				psh.nPages = nPageIndex;
				psh.ppsp = (LPCPROPSHEETPAGE) &psp;
				
				PropertySheet(&psh);
			};
			return TRUE;
		}
		break;
	}
	
	return FALSE;
}

void osRunAboutDialog(char *appName, char *appVersion, char *copyright, char *comments, char *authors, char *documenters, char *translator_credits, BitmapHandle bitmap, WindowHandle owner)
{
	char *s;
    int nDlgWidth, nDlgHeight;
    SIZE sz1, sz2, sz3, bmpSize;
    DlgInfo fi;

	if (!owner) owner = ghWndFrame;
	
	if (!InitDlgInfo(&fi, bitmap, authors, documenters, translator_credits))
		return;
		
	s = (char *) EnsureAtLeastNWords(&fi, (strlen(appName)+strlen(appVersion)+3)/2);
	strcpy(s,appName);
	strcat(s," ");
	strcat(s,appVersion);
	
	GetTextSizeInDlgUnit(&fi,s,         &sz1, TRUE);
	GetTextSizeInDlgUnit(&fi,comments,  &sz2, FALSE);
	GetTextSizeInDlgUnit(&fi,copyright, &sz3, FALSE);
	GetBmpSizeInDlgUnit (&fi,bitmap,    &bmpSize);
	
	nDlgWidth  = bmpSize.cx + max(max(sz1.cx,max(sz2.cx,sz3.cx)),50)+6;
	nDlgHeight = max(bmpSize.cy,sz1.cy+(sz2.cy+sz3.cy)+20)+30;

	ResetBufferPos(&fi);
	
	// start to fill in the dlgtemplate information. Addressing by WORDs
	StoreDWord(&fi, WS_CAPTION | WS_SYSMENU | DS_CENTER | DS_MODALFRAME); // lStyle
	StoreDWord(&fi, 0);                                            // lExtendedStyle
	StoreWord(&fi, 7);                                             // NumberOfItems
	StoreRect(&fi, 0, 0, nDlgWidth, nDlgHeight);                   // rect
	StoreWord(&fi, 0);                                             // Menu (empty)
	StoreWord(&fi, 0);   	                                       // Default dialog class
	StoreStringZ(&fi, "About ");
	StoreString(&fi, appName);
		
	// start fill in the control templates	
	AlignBufferPos(&fi);
	
	// icon
	StoreDWord(&fi, WS_VISIBLE | WS_CHILD | SS_BITMAP | SS_CENTERIMAGE); // lStyle
	StoreDWord(&fi, 0);                                             // lExtendedStyle
	StoreRect(&fi, 2, 2,
	               bmpSize.cx, bmpSize.cy);                         // rect
	StoreWord(&fi, 100);                                            // control id
	StoreDWord(&fi,0x0082FFFF);                                     // control class 'STATIC'
	StoreWord(&fi, 0);                                              // empty title
	StoreWord(&fi, 0);

	AlignBufferPos(&fi);
	
	// app name & version
	StoreDWord(&fi, WS_VISIBLE | WS_CHILD | SS_NOPREFIX | SS_LEFT); // lStyle
	StoreDWord(&fi, 0);                                             // lExtendedStyle
	StoreRect(&fi, bmpSize.cx+4,2,
	               nDlgWidth-bmpSize.cx-2,sz1.cy);                  // rect
	StoreWord(&fi, 101);                                            // control id
	StoreDWord(&fi, 0x0082FFFF);                                    // control class 'STATIC'
	StoreStringZ(&fi, appName);
	StoreWord(&fi, ' ');
	StoreString(&fi, appVersion);
	StoreWord(&fi, 0);
	
	AlignBufferPos(&fi);
	
	// comments
	StoreDWord(&fi, WS_VISIBLE | WS_CHILD | SS_NOPREFIX | SS_LEFT); // lStyle
	StoreDWord(&fi, 0);                                             // lExtendedStyle
	StoreRect(&fi, bmpSize.cx+4, sz1.cy+3,
	               nDlgWidth-bmpSize.cx-2,sz2.cy);                  // rect
	StoreWord(&fi, 102);                                            // control id
	StoreDWord(&fi, 0x0082FFFF);                                    // control class 'STATIC'
	StoreString(&fi, comments);
	StoreWord(&fi, 0);
	
	AlignBufferPos(&fi);
	
	// copyright
	StoreDWord(&fi, WS_VISIBLE | WS_CHILD | SS_NOPREFIX | SS_LEFT); // lStyle
	StoreDWord(&fi, 0);                                             // lExtendedStyle
	StoreRect(&fi, bmpSize.cx+4,sz1.cy+sz2.cy+4,
	               nDlgWidth-bmpSize.cx-2,sz3.cy);                  // rect
	StoreWord(&fi, 103);                                            // control id
	StoreDWord(&fi, 0x0082FFFF);                                    // control class 'STATIC'
	StoreString(&fi, copyright);
	StoreDWord(&fi, 0);
	
	AlignBufferPos(&fi);
		
	// Credits button
	if (authors || documenters || translator_credits)
	  StoreDWord(&fi, WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON);       // lStyle ( visible )
	else
	  StoreDWord(&fi,              WS_CHILD | BS_PUSHBUTTON);       // lStyle ( invisible )
	StoreDWord(&fi, 0);                                             // lExtendedStyle
	StoreRect(&fi, nDlgWidth-52, sz1.cy+sz2.cy+sz3.cy+5,
	               50, 16);                                         // rect
	StoreWord(&fi, IDC_CREDITS);                                    // control id
	StoreDWord(&fi, 0x0080FFFF);                                    // control class 'BUTTON'
	StoreString(&fi, "Credits...");
	StoreWord(&fi, 0);
	
	AlignBufferPos(&fi);
	
	// horizontal separator line
	StoreDWord(&fi, WS_VISIBLE | WS_CHILD | BS_GROUPBOX);           // lStyle
	StoreDWord(&fi, 0);                                             // lExtendedStyle
	StoreRect(&fi, 2, max(bmpSize.cy,sz1.cy+sz2.cy+sz3.cy+20)+2,
	               nDlgWidth-4, 4);                                 // rect
	StoreWord(&fi, 104);                                            // control id
	StoreDWord(&fi, 0x0080FFFF);                                    // control class 'BUTTON'
	StoreWord(&fi, 0);
	StoreWord(&fi, 0);
	
	AlignBufferPos(&fi);
	
	// OK button
	StoreDWord(&fi, WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON);      // lStyle
	StoreDWord(&fi, 0);                                             // lExtendedStyle
	StoreRect(&fi, nDlgWidth-52, max(bmpSize.cy,sz1.cy+sz2.cy+sz3.cy+20)+10, 
	               50, 16);                                         // rect
	StoreWord(&fi, IDOK);                                           // control id
	StoreDWord(&fi, 0x0080FFFF);                                    // control class 'BUTTON'
	StoreString(&fi, "OK");
	StoreWord(&fi, 0);
	
	DialogBoxIndirectParam(ghModule, (LPCDLGTEMPLATE) fi.pBuffer, owner, HAboutDialogFunction, (LPARAM) &fi);
	
	FreeDlgInfo(&fi);
}
