#include "Types.h"
#include "Window.h"
#include "Internals.h"
#include "Handlers_stub.h"
	
#include <wctype.h>
#include <commctrl.h>

HMODULE ghModule = NULL;
HWND ghWndFrame = NULL;

void *rmalloc (DWORD bytes)
{
	void *ptr = malloc (bytes);

	if (!ptr)
	{
		printf("rmalloc failed!\n");
		exit(1);
	}

	return ptr;
}

void rfree (void *ptr)
{
	free(ptr);
}

extern LRESULT CALLBACK HSDIFrameFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HSDIWindowFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HMDIFrameFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HMDIWindowFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HCompoundControlFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HDialogFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HDockBarFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HToolBarFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HNotebookFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HNotebookPageFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HGroupBoxFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HCheckListBoxFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HSplitterFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HEditFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HTreeListFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern LRESULT CALLBACK HWebViewFunction(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

extern WNDPROC DefToolBarProc;
extern WNDPROC DefTabCtrlProc;
extern WNDPROC DefGroupBoxProc;
extern WNDPROC DefCheckListBoxProc;
extern WNDPROC DefEditCtrlProc;

static void parseAppNameVersion(PortString appTitle,PortString appVersion)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	int appNameLen;
	LPWSTR s;

	s = appVersion;
	while (*s != L' ' && *s != 0) s++;

	if (*s == L' ')
	{
		appNameLen = s-appVersion;
		pFrameData->lpszAppName = malloc((appNameLen+1)*sizeof(wchar_t));
		memcpy(pFrameData->lpszAppName, appVersion, appNameLen);
		pFrameData->lpszAppName[appNameLen] = 0;

		while (*s == L' ') s++;
		pFrameData->lpszAppVersion = psdup(s);
	}
	else
	{
		pFrameData->lpszAppName    = psdup(appTitle);
		pFrameData->lpszAppVersion = psdup(appVersion);
	}

	s = pFrameData->lpszAppName;
	appNameLen = 0;
	while (*s)
	{
		if (iswalnum(*s))
			pFrameData->lpszAppName[appNameLen++] = *s++;
		else
			s++;
	}
	pFrameData->lpszAppName[appNameLen] = 0;
}

WindowHandle checkWindow(HWND hWnd, char *className)
{
	HWND hParent;

	if (!hWnd)
	{
		printf("Failed to create window %s", className);
		exit(1);
	}

	SendMessage(hWnd, WM_SETFONT, (WPARAM)GetStockObject(DEFAULT_GUI_FONT), MAKELPARAM (TRUE,0));

	hParent = GetParent(hWnd);
	if (hParent && !osGetWindowEnabled(hParent))
		EnableWindow(hWnd, FALSE);

	return hWnd;
}

extern void doneGdiPlus();

void osStart(PortString appTitle, PortString appVersion, int DocumentInterface, OsInitFunc initFunc)
{
	if (!ghModule)
	{
		WNDCLASSW wc;
		INITCOMMONCONTROLSEX icc;

		ghModule = GetModuleHandle(NULL);

		// SDIFrame class
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HSDIFrameFunction;
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 0;
		wc.hInstance     = ghModule;
		wc.hIcon         = LoadIcon (NULL, IDI_APPLICATION);
		wc.hCursor       = LoadCursor (NULL, IDC_ARROW);
		wc.hbrBackground = GetSysColorBrush(COLOR_APPWORKSPACE);
		wc.lpszMenuName  = NULL;
		wc.lpszClassName = L"HSDIFRAME";
		RegisterClassW(&wc);

		// SDIWindow class
 		wc.style         = CS_DBLCLKS;
    	wc.lpfnWndProc   = HSDIWindowFunction;
    	wc.cbClsExtra    = 0;
    	wc.cbWndExtra    = 0;
    	wc.hInstance     = ghModule;
    	wc.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
    	wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
    	wc.hbrBackground = GetSysColorBrush(COLOR_WINDOW);
    	wc.lpszMenuName  = NULL;
    	wc.lpszClassName = L"HSDIWINDOW";
		RegisterClassW(&wc);

		// Dialog class
 		wc.style         = CS_DBLCLKS;
    	wc.lpfnWndProc   = HDialogFunction;
    	wc.cbClsExtra    = 0;
    	wc.cbWndExtra    = DLGWINDOWEXTRA;
    	wc.hInstance     = ghModule;
    	wc.hIcon         = NULL;
    	wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
    	wc.hbrBackground = GetSysColorBrush(COLOR_3DFACE);
    	wc.lpszMenuName  = NULL;
    	wc.lpszClassName = L"HDIALOG";
		RegisterClassW(&wc);

		// MDIFrame class
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HMDIFrameFunction;
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 0;
		wc.hInstance     = ghModule;
		wc.hIcon         = LoadIcon (NULL, IDI_APPLICATION);
		wc.hCursor       = LoadCursor (NULL, IDC_ARROW);
		wc.hbrBackground = GetSysColorBrush(COLOR_APPWORKSPACE);
		wc.lpszMenuName  = NULL;
		wc.lpszClassName = L"HMDIFRAME";
		RegisterClassW(&wc);

		// MDIWindow class
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HMDIWindowFunction;
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 0;
		wc.hInstance     = ghModule;
		wc.hIcon         = LoadIcon (NULL, IDI_APPLICATION);
		wc.hCursor       = LoadCursor (NULL, IDC_ARROW);
		wc.hbrBackground = GetSysColorBrush(COLOR_WINDOW);
		wc.lpszMenuName  = NULL;
		wc.lpszClassName = L"HMDIWINDOW";
		RegisterClassW(&wc);

		// CompoundControl class
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HCompoundControlFunction;
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 0;
		wc.hInstance     = ghModule;
		wc.hIcon         = NULL;
		wc.hCursor       = LoadCursor (NULL, IDC_ARROW);
		wc.hbrBackground = GetSysColorBrush(COLOR_WINDOW);
		wc.lpszMenuName  = NULL;
		wc.lpszClassName = L"HCOMPOUND";
		RegisterClassW(&wc);

		// GroupBox class
		GetClassInfoW(ghModule, L"BUTTON", &wc);
		DefGroupBoxProc  = wc.lpfnWndProc;
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HGroupBoxFunction;
		wc.lpszClassName = L"HGROUPBOX";
		RegisterClassW(&wc);

		// GroupBox class
		GetClassInfoW(ghModule, L"LISTBOX", &wc);
		DefCheckListBoxProc = wc.lpfnWndProc;
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HCheckListBoxFunction;
		wc.lpszClassName = L"HCHECKLISTBOX";
		RegisterClassW(&wc);

		// DockBar class
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HDockBarFunction;
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 0;
		wc.hInstance     = ghModule;
		wc.hIcon         = NULL;
		wc.hCursor       = NULL;
		wc.hbrBackground = GetSysColorBrush(COLOR_BTNFACE);
		wc.lpszMenuName  = NULL;
		wc.lpszClassName = L"HDOCKBAR";
		RegisterClassW(&wc);

		// ToolBar class (subclass of the standard ToolBar class)
		GetClassInfoW(ghModule, TOOLBARCLASSNAMEW, &wc);
		DefToolBarProc = wc.lpfnWndProc;
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HToolBarFunction;
		wc.lpszClassName = L"HTOOLBAR";
		RegisterClassW(&wc);

		// Notebook class (subclass of the standard TabCtrl class)
		GetClassInfoW(ghModule, WC_TABCONTROLW, &wc);
		DefTabCtrlProc = wc.lpfnWndProc;
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HNotebookFunction;
		wc.lpszClassName = L"HNOTEBOOK";
		RegisterClassW(&wc);

		// NotebookPage class
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HNotebookPageFunction;
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 0;
		wc.hInstance     = ghModule;
		wc.hIcon         = NULL;
		wc.hCursor       = NULL;
		wc.hbrBackground = GetSysColorBrush(COLOR_BTNFACE);
		wc.lpszMenuName  = NULL;
		wc.lpszClassName = L"HNOTEBOOKPAGE";
		RegisterClassW(&wc);

		// Splitter class
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HSplitterFunction;
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 0;
		wc.hInstance     = ghModule;
		wc.hIcon         = NULL;
		wc.hCursor       = LoadCursor (NULL, IDC_ARROW);
		wc.hbrBackground = GetSysColorBrush(COLOR_WINDOW);
		wc.lpszMenuName  = NULL;
		wc.lpszClassName = L"HSPLITTER";
		RegisterClassW(&wc);

		// HEDIT class
		GetClassInfoW(ghModule, L"EDIT", &wc);
		DefEditCtrlProc  = wc.lpfnWndProc;
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HEditFunction;
		wc.lpszClassName = L"HEDIT";
		RegisterClassW(&wc);

		wc.style		= CS_DBLCLKS;
		wc.lpfnWndProc	= HTreeListFunction;
		wc.cbClsExtra	= 0;
		wc.cbWndExtra	= sizeof(void*);
		wc.hInstance	= ghModule;
		wc.hIcon		= NULL;
		wc.hCursor		= LoadCursor(NULL,IDC_ARROW);
		wc.hbrBackground= NULL;
		wc.lpszMenuName	= NULL;
		wc.lpszClassName= L"HTreeList";
		RegisterClassW(&wc);

		// CompoundControl class
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HWebViewFunction;
		wc.cbClsExtra    = 0;
		wc.cbWndExtra    = 0;
		wc.hInstance     = ghModule;
		wc.hIcon         = NULL;
		wc.hCursor       = LoadCursor (NULL, IDC_ARROW);
		wc.hbrBackground = NULL;
		wc.lpszMenuName  = NULL;
		wc.lpszClassName = L"HWebView";
		RegisterClassW(&wc);

		icc.dwSize = sizeof(icc);
		icc.dwICC = ICC_WIN95_CLASSES | ICC_DATE_CLASSES;
		InitCommonControlsEx(&icc);
		
		OleInitialize(NULL);
	}

	if (ghWndFrame == NULL)
	{
		if (DocumentInterface == 1)
		{
			ghWndFrame = CreateWindowW( L"HSDIFRAME",
										appTitle,
										WS_OVERLAPPEDWINDOW,
										CW_USEDEFAULT,CW_USEDEFAULT,
										CW_USEDEFAULT,CW_USEDEFAULT,
										NULL,
										NULL,
										(HANDLE) ghModule,
										NULL
									  );
		}
		else
		{
			ghWndFrame = CreateWindowW( L"HMDIFRAME",
										appTitle,
										WS_OVERLAPPEDWINDOW,
										CW_USEDEFAULT,CW_USEDEFAULT,
										CW_USEDEFAULT,CW_USEDEFAULT,
										NULL,
										NULL,
										(HANDLE) ghModule,
										NULL
									  );
		}

		parseAppNameVersion(appTitle,appVersion);

		RestoreWindowState(ghWndFrame, SW_NORMAL);
		UpdateWindow(ghWndFrame);
	}

	if (initFunc != NULL) {
		initFunc();
		hs_free_fun_ptr(initFunc);
		initFunc = NULL;
	}

	MSG msg;
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	while (GetMessage(&msg, NULL, 0, 0) != 0)
	{
		if ((pFrameData->DocumentInterface != 2 || !TranslateMDISysAccel(pFrameData->hClientWnd, &msg)) &&
		    !TranslateAccelerator(pFrameData->hClientWnd, getAccelTableFromMap(pFrameData->pActionsMap), &msg))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	doneGdiPlus();
};

void osQuit()
{
	OleUninitialize();
	DestroyWindow(ghWndFrame);
	ghWndFrame = NULL;
}

#ifdef WINE_TARGET
#include "HsFFI.h"
#include "Rts.h"
#include "wine/library.h"

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine, int nShowCmd)
{
    RtsConfig conf = defaultRtsConfig;

    extern StgClosure ZCMain_main_closure;
    
    hs_main(__wine_main_argc, __wine_main_argv, &ZCMain_main_closure, conf);
}
#endif
