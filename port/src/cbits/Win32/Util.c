#include "Types.h"
#include "Window.h"
#include "Internals.h"
#include "Handlers_stub.h"

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

extern WNDPROC DefToolBarProc;
extern WNDPROC DefTabCtrlProc;
extern WNDPROC DefGroupBoxProc;
extern WNDPROC DefCheckListBoxProc;

static void parseAppNameVersion(char *appTitle,char *appVersion)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	int appNameLen;
	char *s;

	s = appVersion;
	while (*s != ' ' && *s != 0) s++;

	if (*s == ' ')
	{
		appNameLen = s-appVersion;
		pFrameData->lpszAppName = malloc(appNameLen+1);
		memcpy(pFrameData->lpszAppName, appVersion, appNameLen);
		pFrameData->lpszAppName[appNameLen] = 0;

		while (*s == ' ') s++;
		pFrameData->lpszAppVersion = strdup(s);
	}
	else
	{
		pFrameData->lpszAppName    = strdup(appTitle);
		pFrameData->lpszAppVersion = strdup(appVersion);
	}

	s = pFrameData->lpszAppName;
	appNameLen = 0;
	while (*s)
	{
		if (isalnum(*s))
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

void osStart(char *appTitle, char *appVersion, int DocumentInterface, OsInitFunc initFunc)
{
	if (!ghModule)
	{
		WNDCLASS wc;
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
		wc.lpszClassName = "HSDIFRAME";
		RegisterClass(&wc);

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
    	wc.lpszClassName = "HSDIWINDOW";
		RegisterClass(&wc);

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
    	wc.lpszClassName = "HDIALOG";
		RegisterClass(&wc);

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
		wc.lpszClassName = "HMDIFRAME";
		RegisterClass(&wc);

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
		wc.lpszClassName = "HMDIWINDOW";
		RegisterClass(&wc);

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
		wc.lpszClassName = "HCOMPOUND";
		RegisterClass(&wc);

		// GroupBox class
		GetClassInfo(ghModule, "BUTTON", &wc);
		DefGroupBoxProc  = wc.lpfnWndProc;
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HGroupBoxFunction;
		wc.lpszClassName = "HGROUPBOX";
		RegisterClass(&wc);

		// GroupBox class
		GetClassInfo(ghModule, "LISTBOX", &wc);
		DefCheckListBoxProc = wc.lpfnWndProc;
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HCheckListBoxFunction;
		wc.lpszClassName = "HCHECKLISTBOX";
		RegisterClass(&wc);

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
		wc.lpszClassName = "HDOCKBAR";
		RegisterClass(&wc);

		// ToolBar class (subclass of the standard ToolBar class)
		GetClassInfo(ghModule, TOOLBARCLASSNAME, &wc);
		DefToolBarProc = wc.lpfnWndProc;
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HToolBarFunction;
		wc.lpszClassName = "HTOOLBAR";
		RegisterClass(&wc);

		// Notebook class (subclass of the standard TabCtrl class)
		GetClassInfo(ghModule, WC_TABCONTROL, &wc);
		DefTabCtrlProc = wc.lpfnWndProc;
		wc.style         = CS_DBLCLKS;
		wc.lpfnWndProc   = HNotebookFunction;
		wc.lpszClassName = "HNOTEBOOK";
		RegisterClass(&wc);

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
		wc.lpszClassName = "HNOTEBOOKPAGE";
		RegisterClass(&wc);

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
		wc.lpszClassName = "HSPLITTER";
		RegisterClass(&wc);

		icc.dwSize = sizeof(icc);
		icc.dwICC = ICC_WIN95_CLASSES | ICC_DATE_CLASSES;
		InitCommonControlsEx(&icc);
	}

	if (ghWndFrame == NULL)
	{
		if (DocumentInterface == 1)
		{
			ghWndFrame = CreateWindow ( "HSDIFRAME",
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
			ghWndFrame = CreateWindow ( "HMDIFRAME",
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
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

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
	DestroyWindow(ghWndFrame);
	ghWndFrame = NULL;
}

