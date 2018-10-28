#define INITGUID
#include <guiddef.h>

#include "WebView.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include <exdisp.h>       /* Defines of stuff like IWebBrowser2. This is an include file with Visual C 6 and above */
#include <mshtml.h>       /* Defines of stuff like IHTMLDocument2. This is an include file with Visual C 6 and above */
#include <mshtmhst.h> /* Defines of stuff like IDocHostUIHandler. This is an include file with Visual C 6 and above */

// Passed to an app's window procedure (as a WM_NOTIFY message) whenever an
// action has occurred on the web page (and the app has asked to be informed
// of that specific action)
typedef struct {
    NMHDR           nmhdr;
    IHTMLEventObj * htmlEvent;
    LPCTSTR         eventStr;
} WEBPARAMS; 

// Our _IDispatchEx struct. This is just an IDispatch with some
// extra fields appended to it for our use in storing extra
// info we need for the purpose of reacting to events that happen
// to some element on a web page.
typedef struct {
    IDispatch       dispatchObj;    // The mandatory IDispatch.
    DWORD           refCount;       // Our reference count.
    IHTMLWindow2 *  htmlWindow2;    // Where we store the IHTMLWindow2 so that our IDispatch's Invoke() can get it.
    HWND            hwnd;           // The window hosting the browser page. Our IDispatch's Invoke() sends messages when an event of interest occurs.
    short           id;             // Any numeric value of your choosing that you wish to associate with this IDispatch.
    unsigned short  extraSize;      // Byte size of any extra fields prepended to this struct.
    IUnknown        *object;        // Some object associated with the web page element this IDispatch is for.
    void            *userdata;      // An extra pointer.
} _IDispatchEx;

static IHTMLElement * WINAPI GetWebElement(HWND, IHTMLDocument2 *, const WCHAR *, INT);
static IHTMLElement * WINAPI GetWebSrcElement(IHTMLEventObj *);


#define WEBPAGE_GOBACK      0
#define WEBPAGE_GOFORWARD   1
#define WEBPAGE_GOHOME      2
#define WEBPAGE_SEARCH      3
#define WEBPAGE_REFRESH     4
#define WEBPAGE_STOP        5

static void WINAPI DoPageAction(HWND, DWORD);

#define WORS_SUCCESS    0
#define WORS_TIMEOUT    -1
#define WORS_DESTROYED  -2

static HRESULT WINAPI WaitOnReadyState(HWND, READYSTATE, DWORD, IWebBrowser2 *);

static HRESULT WINAPI GetWebPtrs(HWND, IWebBrowser2 **, IHTMLDocument2 **);

static HRESULT WINAPI SetWebReturnValue(IHTMLEventObj *, BOOL);

static void WINAPI FreeWebEvtHandler(IDispatch *);

static IDispatch * WINAPI CreateWebEvtHandler(HWND, IHTMLDocument2 *, DWORD, long, IUnknown *, void *);

// Used by UI_TranslateUrl().  These can be global because we never change them.
static const wchar_t Blank[] = {L"about:blank"};

#define XV2(x) (x).__VARIANT_NAME_1.__VARIANT_NAME_2
#define XV3(x) (x).__VARIANT_NAME_1.__VARIANT_NAME_2.__VARIANT_NAME_3

static unsigned char _IID_IHTMLWindow3[] = {0xae, 0xf4, 0x50, 0x30, 0xb5, 0x98, 0xcf, 0x11, 0xbb, 0x82, 0x00, 0xaa, 0x00, 0xbd, 0xce, 0x0b};


// Our IOleInPlaceFrame functions that the browser may call
static HRESULT STDMETHODCALLTYPE Frame_QueryInterface(IOleInPlaceFrame *, REFIID, LPVOID *);
static ULONG   STDMETHODCALLTYPE Frame_AddRef(IOleInPlaceFrame *);
static ULONG   STDMETHODCALLTYPE Frame_Release(IOleInPlaceFrame *);
static HRESULT STDMETHODCALLTYPE Frame_GetWindow(IOleInPlaceFrame *, HWND *);
static HRESULT STDMETHODCALLTYPE Frame_ContextSensitiveHelp(IOleInPlaceFrame *, BOOL);
static HRESULT STDMETHODCALLTYPE Frame_GetBorder(IOleInPlaceFrame *, LPRECT);
static HRESULT STDMETHODCALLTYPE Frame_RequestBorderSpace(IOleInPlaceFrame *, LPCBORDERWIDTHS);
static HRESULT STDMETHODCALLTYPE Frame_SetBorderSpace(IOleInPlaceFrame *, LPCBORDERWIDTHS);
static HRESULT STDMETHODCALLTYPE Frame_SetActiveObject(IOleInPlaceFrame *, IOleInPlaceActiveObject *, LPCOLESTR);
static HRESULT STDMETHODCALLTYPE Frame_InsertMenus(IOleInPlaceFrame *, HMENU, LPOLEMENUGROUPWIDTHS);
static HRESULT STDMETHODCALLTYPE Frame_SetMenu(IOleInPlaceFrame *, HMENU, HOLEMENU, HWND);
static HRESULT STDMETHODCALLTYPE Frame_RemoveMenus(IOleInPlaceFrame *, HMENU);
static HRESULT STDMETHODCALLTYPE Frame_SetStatusText(IOleInPlaceFrame *, LPCOLESTR);
static HRESULT STDMETHODCALLTYPE Frame_EnableModeless(IOleInPlaceFrame *, BOOL);
static HRESULT STDMETHODCALLTYPE Frame_TranslateAccelerator(IOleInPlaceFrame *, LPMSG, WORD);

// Our IOleInPlaceFrame VTable. This is the array of pointers to the above functions in our C
// program that the browser may call in order to interact with our frame window that contains
// the browser object. We must define a particular set of functions that comprise the
// IOleInPlaceFrame set of functions (see above), and then stuff pointers to those functions
// in their respective 'slots' in this table. We want the browser to use this VTable with our
// IOleInPlaceFrame structure.
static IOleInPlaceFrameVtbl MyIOleInPlaceFrameTable = {
			Frame_QueryInterface,
			Frame_AddRef,
			Frame_Release,
			Frame_GetWindow,
			Frame_ContextSensitiveHelp,
			Frame_GetBorder,
			Frame_RequestBorderSpace,
			Frame_SetBorderSpace,
			Frame_SetActiveObject,
			Frame_InsertMenus,
			Frame_SetMenu,
			Frame_RemoveMenus,
			Frame_SetStatusText,
			Frame_EnableModeless,
			Frame_TranslateAccelerator
		};

// We need to return an IOleInPlaceFrame struct to the browser object. And one of our IOleInPlaceFrame
// functions (Frame_GetWindow) is going to need to access our window handle. So let's create our own
// struct that starts off with an IOleInPlaceFrame struct (and that's important -- the IOleInPlaceFrame
// struct *must* be first), and then has an extra data member where we can store our own window's HWND.
//
// And because we may want to create multiple windows, each hosting its own browser object (to
// display its own web page), then we need to create a IOleInPlaceFrame struct for each window. So,
// we're not going to declare our IOleInPlaceFrame struct globally. We'll allocate it later using
// GlobalAlloc, and then stuff the appropriate HWND in it then, and also stuff a pointer to
// MyIOleInPlaceFrameTable in it. But let's just define it here.
typedef struct {
    IOleInPlaceFrame    frame;      // The IOleInPlaceFrame must be first!

    ///////////////////////////////////////////////////
    // Here you add any extra variables that you need
    // to access in your IOleInPlaceFrame functions.
    // You don't want those functions to access global
    // variables, because then you couldn't use more
    // than one browser object. (ie, You couldn't have
    // multiple windows, each with its own embedded
    // browser object to display a different web page).
    //
    // So here is where I added my extra HWND that my
    // IOleInPlaceFrame function Frame_GetWindow() needs
    // to access.
    ///////////////////////////////////////////////////
    HWND                window;
} _IOleInPlaceFrameEx;






// Our IOleClientSite functions that the browser may call
static HRESULT STDMETHODCALLTYPE Site_QueryInterface(IOleClientSite *, REFIID, void **);
static ULONG   STDMETHODCALLTYPE Site_AddRef(IOleClientSite *);
static ULONG   STDMETHODCALLTYPE Site_Release(IOleClientSite *);
static HRESULT STDMETHODCALLTYPE Site_SaveObject(IOleClientSite *);
static HRESULT STDMETHODCALLTYPE Site_GetMoniker(IOleClientSite *, DWORD, DWORD, IMoniker **);
static HRESULT STDMETHODCALLTYPE Site_GetContainer(IOleClientSite *, LPOLECONTAINER *);
static HRESULT STDMETHODCALLTYPE Site_ShowObject(IOleClientSite *);
static HRESULT STDMETHODCALLTYPE Site_OnShowWindow(IOleClientSite *, BOOL);
static HRESULT STDMETHODCALLTYPE Site_RequestNewObjectLayout(IOleClientSite *);

// Our IOleClientSite VTable. This is the array of pointers to the above functions in our C
// program that the browser may call in order to interact with our frame window that contains
// the browser object. We must define a particular set of functions that comprise the
// IOleClientSite set of functions (see above), and then stuff pointers to those functions
// in their respective 'slots' in this table. We want the browser to use this VTable with our
// IOleClientSite structure.
static IOleClientSiteVtbl MyIOleClientSiteTable = {
			Site_QueryInterface,
			Site_AddRef,
			Site_Release,
			Site_SaveObject,
			Site_GetMoniker,
			Site_GetContainer,
			Site_ShowObject,
			Site_OnShowWindow,
			Site_RequestNewObjectLayout
		};






// Our IDocHostUIHandler functions that the browser may call
static HRESULT STDMETHODCALLTYPE UI_QueryInterface(IDocHostUIHandler *, REFIID, void **);
static ULONG   STDMETHODCALLTYPE UI_AddRef(IDocHostUIHandler *);
static ULONG   STDMETHODCALLTYPE UI_Release(IDocHostUIHandler *);
static HRESULT STDMETHODCALLTYPE UI_ShowContextMenu(IDocHostUIHandler *, DWORD, POINT *, IUnknown *, IDispatch *);
static HRESULT STDMETHODCALLTYPE UI_GetHostInfo(IDocHostUIHandler *, DOCHOSTUIINFO *);
static HRESULT STDMETHODCALLTYPE UI_ShowUI(IDocHostUIHandler *, DWORD, IOleInPlaceActiveObject *, IOleCommandTarget *, IOleInPlaceFrame *, IOleInPlaceUIWindow *);
static HRESULT STDMETHODCALLTYPE UI_HideUI(IDocHostUIHandler *);
static HRESULT STDMETHODCALLTYPE UI_UpdateUI(IDocHostUIHandler *);
static HRESULT STDMETHODCALLTYPE UI_EnableModeless(IDocHostUIHandler *, BOOL);
static HRESULT STDMETHODCALLTYPE UI_OnDocWindowActivate(IDocHostUIHandler *, BOOL);
static HRESULT STDMETHODCALLTYPE UI_OnFrameWindowActivate(IDocHostUIHandler *, BOOL);
static HRESULT STDMETHODCALLTYPE UI_ResizeBorder(IDocHostUIHandler *, LPCRECT, IOleInPlaceUIWindow  *, BOOL);
static HRESULT STDMETHODCALLTYPE UI_TranslateAccelerator(IDocHostUIHandler *, LPMSG, const GUID *, DWORD);
static HRESULT STDMETHODCALLTYPE UI_GetOptionKeyPath(IDocHostUIHandler *, LPOLESTR *, DWORD);
static HRESULT STDMETHODCALLTYPE UI_GetDropTarget(IDocHostUIHandler *, IDropTarget *, IDropTarget **);
static HRESULT STDMETHODCALLTYPE UI_GetExternal(IDocHostUIHandler *, IDispatch **);
static HRESULT STDMETHODCALLTYPE UI_TranslateUrl(IDocHostUIHandler *, DWORD, OLECHAR *, OLECHAR  **);
static HRESULT STDMETHODCALLTYPE UI_FilterDataObject(IDocHostUIHandler *, IDataObject *, IDataObject **);

// Our IDocHostUIHandler VTable. This is the array of pointers to the above functions in our C
// program that the browser may call in order to replace/set certain user interface considerations
// (such as whether to display a pop-up context menu when the user right-clicks on the embedded
// browser object). We must define a particular set of functions that comprise the
// IDocHostUIHandler set of functions (see above), and then stuff pointers to those functions
// in their respective 'slots' in this table. We want the browser to use this VTable with our
// IDocHostUIHandler structure.
static IDocHostUIHandlerVtbl MyIDocHostUIHandlerTable = {
			UI_QueryInterface,
			UI_AddRef,
			UI_Release,
			UI_ShowContextMenu,
			UI_GetHostInfo,
			UI_ShowUI,
			UI_HideUI,
			UI_UpdateUI,
			UI_EnableModeless,
			UI_OnDocWindowActivate,
			UI_OnFrameWindowActivate,
			UI_ResizeBorder,
			UI_TranslateAccelerator,
			UI_GetOptionKeyPath,
			UI_GetDropTarget,
			UI_GetExternal,
			UI_TranslateUrl,
			UI_FilterDataObject
		};

// We'll allocate our IDocHostUIHandler object dynamically with GlobalAlloc() for reasons outlined later.



// Our IOleInPlaceSite functions that the browser may call
static HRESULT STDMETHODCALLTYPE InPlace_QueryInterface(IOleInPlaceSite *, REFIID, void **);
static ULONG   STDMETHODCALLTYPE InPlace_AddRef(IOleInPlaceSite *);
static ULONG   STDMETHODCALLTYPE InPlace_Release(IOleInPlaceSite *);
static HRESULT STDMETHODCALLTYPE InPlace_GetWindow(IOleInPlaceSite *, HWND *);
static HRESULT STDMETHODCALLTYPE InPlace_ContextSensitiveHelp(IOleInPlaceSite *, BOOL);
static HRESULT STDMETHODCALLTYPE InPlace_CanInPlaceActivate(IOleInPlaceSite *);
static HRESULT STDMETHODCALLTYPE InPlace_OnInPlaceActivate(IOleInPlaceSite *);
static HRESULT STDMETHODCALLTYPE InPlace_OnUIActivate(IOleInPlaceSite *);
static HRESULT STDMETHODCALLTYPE InPlace_GetWindowContext(IOleInPlaceSite *, LPOLEINPLACEFRAME *, LPOLEINPLACEUIWINDOW *, LPRECT, LPRECT, LPOLEINPLACEFRAMEINFO);
static HRESULT STDMETHODCALLTYPE InPlace_Scroll(IOleInPlaceSite *, SIZE);
static HRESULT STDMETHODCALLTYPE InPlace_OnUIDeactivate(IOleInPlaceSite *, BOOL);
static HRESULT STDMETHODCALLTYPE InPlace_OnInPlaceDeactivate(IOleInPlaceSite *);
static HRESULT STDMETHODCALLTYPE InPlace_DiscardUndoState(IOleInPlaceSite *);
static HRESULT STDMETHODCALLTYPE InPlace_DeactivateAndUndo(IOleInPlaceSite *);
static HRESULT STDMETHODCALLTYPE InPlace_OnPosRectChange(IOleInPlaceSite *, LPCRECT);

// Our IOleInPlaceSite VTable. This is the array of pointers to the above functions in our C
// program that the browser may call in order to interact with our frame window that contains
// the browser object. We must define a particular set of functions that comprise the
// IOleInPlaceSite set of functions (see above), and then stuff pointers to those functions
// in their respective 'slots' in this table. We want the browser to use this VTable with our
// IOleInPlaceSite structure.
static IOleInPlaceSiteVtbl MyIOleInPlaceSiteTable = {
			InPlace_QueryInterface,
			InPlace_AddRef,
			InPlace_Release,
			InPlace_GetWindow,
			InPlace_ContextSensitiveHelp,
			InPlace_CanInPlaceActivate,
			InPlace_OnInPlaceActivate,
			InPlace_OnUIActivate,
			InPlace_GetWindowContext,
			InPlace_Scroll,
			InPlace_OnUIDeactivate,
			InPlace_OnInPlaceDeactivate,
			InPlace_DiscardUndoState,
			InPlace_DeactivateAndUndo,
			InPlace_OnPosRectChange
		};

// We need to pass our IOleClientSite structure to the browser object's SetClientSite().
// But the browser is also going to ask our IOleClientSite's QueryInterface() to return a pointer to
// our IOleInPlaceSite and/or IDocHostUIHandler structs. So we'll need to have those pointers handy.
// Plus, some of our IOleClientSite and IOleInPlaceSite functions will need to have the HWND to our
// window, and also a pointer to our IOleInPlaceFrame struct. So let's create a single struct that
// has the IOleClientSite, IOleInPlaceSite, IDocHostUIHandler, and IOleInPlaceFrame structs all inside
// it (so we can easily get a pointer to any one from any of those structs' functions). As long as the
// IOleClientSite struct is the very first thing in this custom struct, it's all ok. We can still pass
// it to the browser's SetClientSite() and pretend that it's an ordinary IOleClientSite. We'll call
// this new struct a _IOleClientSiteEx.
//
// And because we may want to create multiple windows, each hosting its own browser object (to
// display its own web page), then we need to create a unique _IOleClientSiteEx struct for
// each window. So, we're not going to declare this struct globally. We'll allocate it later
// using GlobalAlloc, and then initialize the structs within it.

typedef struct {
    IOleInPlaceSite         inplace;    // My IOleInPlaceSite object. Must be first with in _IOleInPlaceSiteEx.

    ///////////////////////////////////////////////////
    // Here you add any extra variables that you need
    // to access in your IOleInPlaceSite functions.
    //
    // So here is where I added my IOleInPlaceFrame
    // struct. If you need extra variables, add them
    // at the end.
    ///////////////////////////////////////////////////
    _IOleInPlaceFrameEx     frame;      // My IOleInPlaceFrame object. Must be first within my _IOleInPlaceFrameEx
} _IOleInPlaceSiteEx;

typedef struct {
    IDocHostUIHandler       ui;         // My IDocHostUIHandler object. Must be first.

    ///////////////////////////////////////////////////
    // Here you add any extra variables that you need
    // to access in your IDocHostUIHandler functions.
    ///////////////////////////////////////////////////
} _IDocHostUIHandlerEx;

typedef struct {
    IOleClientSite          client;     // My IOleClientSite object. Must be first.
    _IOleInPlaceSiteEx      inplace;    // My IOleInPlaceSite object. A convenient place to put it.
    _IDocHostUIHandlerEx    ui;         // My IDocHostUIHandler object. Must be first within my _IDocHostUIHandlerEx.

	IOleObject *browserObject;
} _IOleClientSiteEx;




// We need an IDispatch function in order to receive some "feedback"
// from IE's browser engine as to particular actions (events) that happen.
// For example, we can request that IE inform us when the mouse pointer
// moves over some element on the web page, such as text marked with
// a particular FONT tag. Or we can request that IE inform us when the
// user clicks on the button that submits a FORM's information. Or, we
// can request that we be informed when the user double-clicks anywhere on
// the page (which isn't part of some tag). There are many elements (ie,
// tags) on the typical web page, and each type of element typically
// has many kinds of actions it can report. We can request to be informed
// of only specific actions, with specific elements. But we need an IDispatch
// for that. IE calls our IDispatch's Invoke() function for each action we've
// requested to be informed of.

// Our IDispatch functions that the browser may call
static HRESULT STDMETHODCALLTYPE Dispatch_QueryInterface(IDispatch *, REFIID riid, void **);
static ULONG   STDMETHODCALLTYPE Dispatch_AddRef(IDispatch *);
static ULONG   STDMETHODCALLTYPE Dispatch_Release(IDispatch *);
static HRESULT STDMETHODCALLTYPE Dispatch_GetTypeInfoCount(IDispatch *, unsigned int *);
static HRESULT STDMETHODCALLTYPE Dispatch_GetTypeInfo(IDispatch *, unsigned int, LCID, ITypeInfo **);
static HRESULT STDMETHODCALLTYPE Dispatch_GetIDsOfNames(IDispatch *, REFIID, OLECHAR **, unsigned int, LCID, DISPID *);
static HRESULT STDMETHODCALLTYPE Dispatch_Invoke(IDispatch *, DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, unsigned int *);

// The VTable for our _IDispatchEx object.
static IDispatchVtbl MyIDispatchVtbl = {
			Dispatch_QueryInterface,
			Dispatch_AddRef,
			Dispatch_Release,
			Dispatch_GetTypeInfoCount,
			Dispatch_GetTypeInfo,
			Dispatch_GetIDsOfNames,
			Dispatch_Invoke
		};

// Some misc stuff used by our IDispatch
static const BSTR   OnBeforeOnLoad = L"onbeforeunload";
static const WCHAR  BeforeUnload[] = L"beforeunload";


//////////////////////////////////// My IDocHostUIHandler functions  //////////////////////////////////////
// The browser object asks us for the pointer to our IDocHostUIHandler object by calling our IOleClientSite's
// QueryInterface (ie, Site_QueryInterface) and specifying a REFIID of IID_IDocHostUIHandler.
//
// NOTE: You need at least IE 4.0. Previous versions do not ask for, nor utilize, our IDocHostUIHandler functions.

static HRESULT STDMETHODCALLTYPE UI_QueryInterface(IDocHostUIHandler *This, REFIID riid, LPVOID *ppvObj)
{
    // The browser assumes that our IDocHostUIHandler object is associated with our IOleClientSite
    // object. So it is possible that the browser may call our IDocHostUIHandler's QueryInterface()
    // to ask us to return a pointer to our IOleClientSite, in the same way that the browser calls
    // our IOleClientSite's QueryInterface() to ask for a pointer to our IDocHostUIHandler.
    //
    // Rather than duplicate much of the code in IOleClientSite's QueryInterface, let's just get
    // a pointer to our _IOleClientSiteEx object, substitute it as the 'This' arg, and call our
    // our IOleClientSite's QueryInterface. Note that since our _IDocHostUIHandlerEx is embedded right
    // inside our _IOleClientSiteEx, and comes immediately after the _IOleInPlaceSiteEx, we can employ
    // the following trickery to get the pointer to our _IOleClientSiteEx.
    return(Site_QueryInterface((IOleClientSite *)((char *)This - sizeof(IOleClientSite) - sizeof(_IOleInPlaceSiteEx)), riid, ppvObj));
}

static ULONG STDMETHODCALLTYPE UI_AddRef(IDocHostUIHandler *This)
{
    return(1);
}

static ULONG STDMETHODCALLTYPE UI_Release(IDocHostUIHandler *This)
{
    return(1);
}

// Called when the browser object is about to display its context menu.
static HRESULT STDMETHODCALLTYPE UI_ShowContextMenu(IDocHostUIHandler *This, DWORD dwID, POINT *ppt, IUnknown *pcmdtReserved, IDispatch *pdispReserved)
{
    POINT           pt;

    GetCursorPos(&pt);

    // If desired, we can pop up our own custom context menu here. But instead, let's
    // just post a WM_CONTENTMENU message to the window hosting the web browser (stored in our
    // _IOleInPlaceFrameEx). Then, we'll tell the browser not to bring up its own context menu,
    // by returning S_OK.
    PostMessageW(((_IOleInPlaceSiteEx *)((char *)This - sizeof(_IOleInPlaceSiteEx)))->frame.window, WM_CONTEXTMENU, (WPARAM)pt.x, pt.y);
    return(S_OK);
}

// Called at initialization of the browser object UI. We can set various features of the browser object here.
static HRESULT STDMETHODCALLTYPE UI_GetHostInfo(IDocHostUIHandler *This, DOCHOSTUIINFO *pInfo)
{
    pInfo->cbSize = sizeof(DOCHOSTUIINFO);

    // Set some flags. We don't want any 3D border. You can do other things like hide
    // the scroll bar (DOCHOSTUIFLAG_SCROLL_NO), disable picture display (DOCHOSTUIFLAG_NOPICS),
    // disable any script running when the page is loaded (DOCHOSTUIFLAG_DISABLE_SCRIPT_INACTIVE),
    // open a site in a new browser window when the user clicks on some link (DOCHOSTUIFLAG_OPENNEWWIN),
    // and lots of other things. See the MSDN docs on the DOCHOSTUIINFO struct passed to us.
    pInfo->dwFlags = DOCHOSTUIFLAG_NO3DBORDER;

    // Set what happens when the user double-clicks on the object. Here we use the default.
    pInfo->dwDoubleClick = DOCHOSTUIDBLCLK_DEFAULT;

    return(S_OK);
}

// Called when the browser object shows its UI. This allows us to replace its menus and toolbars by creating our
// own and displaying them here.
static HRESULT STDMETHODCALLTYPE UI_ShowUI(IDocHostUIHandler *This, DWORD dwID, IOleInPlaceActiveObject *pActiveObject, IOleCommandTarget __RPC_FAR *pCommandTarget, IOleInPlaceFrame __RPC_FAR *pFrame, IOleInPlaceUIWindow *pDoc)
{
    // We've already got our own UI in place so just return S_OK to tell the browser
    // not to display its menus/toolbars. Otherwise we'd return S_FALSE to let it do
    // that.
    return S_OK;
}

// Called when browser object hides its UI. This allows us to hide any menus/toolbars we created in ShowUI.
static HRESULT STDMETHODCALLTYPE UI_HideUI(IDocHostUIHandler *This)
{
    return S_OK;
}

// Called when the browser object wants to notify us that the command state has changed. We should update any
// controls we have that are dependent upon our embedded object, such as "Back", "Forward", "Stop", or "Home"
// buttons.
static HRESULT STDMETHODCALLTYPE UI_UpdateUI(IDocHostUIHandler *This)
{
    // We update our UI in our window message loop so we don't do anything here.
    return S_OK;
}

// Called from the browser object's IOleInPlaceActiveObject object's EnableModeless() function. Also
// called when the browser displays a modal dialog box.
static HRESULT STDMETHODCALLTYPE UI_EnableModeless(IDocHostUIHandler *This, BOOL fEnable)
{
    return S_OK;
}

// Called from the browser object's IOleInPlaceActiveObject object's OnDocWindowActivate() function.
// This informs off of when the object is getting/losing the focus.
static HRESULT STDMETHODCALLTYPE UI_OnDocWindowActivate(IDocHostUIHandler *This, BOOL fActivate)
{
    return S_OK;
}

// Called from the browser object's IOleInPlaceActiveObject object's OnFrameWindowActivate() function.
static HRESULT STDMETHODCALLTYPE UI_OnFrameWindowActivate(IDocHostUIHandler *This, BOOL fActivate)
{
    return S_OK;
}

// Called from the browser object's IOleInPlaceActiveObject object's ResizeBorder() function.
static HRESULT STDMETHODCALLTYPE UI_ResizeBorder(IDocHostUIHandler *This, LPCRECT prcBorder, IOleInPlaceUIWindow *pUIWindow, BOOL fRameWindow)
{
    return S_OK;
}

// Called from the browser object's TranslateAccelerator routines to translate key strokes to commands.
static HRESULT STDMETHODCALLTYPE UI_TranslateAccelerator(IDocHostUIHandler *This, LPMSG lpMsg, const GUID *pguidCmdGroup, DWORD nCmdID)
{
    // We don't intercept any keystrokes, so we do nothing here. But for example, if we wanted to
    // override the TAB key, perhaps do something with it ourselves, and then tell the browser
    // not to do anything with this keystroke, we'd do:
    //
    //  if (pMsg && pMsg->message == WM_KEYDOWN && pMsg->wParam == VK_TAB)
    //  {
    //      // Here we do something as a result of a TAB key press.
    //
    //      // Tell the browser not to do anything with it.
    //      return(S_FALSE);
    //  }
    //
    //  // Otherwise, let the browser do something with this message.
    //  return(S_OK);

    // For our example, we want to make sure that the user can't invoke some key to popup the context
    // menu, so we'll tell it to ignore all msgs.
    return S_FALSE;
}

// Called by the browser object to find where the host wishes the browser to get its options in the registry.
// We can use this to prevent the browser from using its default settings in the registry, by telling it to use
// some other registry key we've setup with the options we want.
static HRESULT STDMETHODCALLTYPE UI_GetOptionKeyPath(IDocHostUIHandler * This, LPOLESTR __RPC_FAR *pchKey, DWORD dw)
{
    // Let the browser use its default registry settings.
    return S_FALSE;
}

// Called by the browser object when it is used as a drop target. We can supply our own IDropTarget object,
// IDropTarget functions, and IDropTarget VTable if we want to determine what happens when someone drags and
// drops some object on our embedded browser object.
static HRESULT STDMETHODCALLTYPE UI_GetDropTarget(IDocHostUIHandler * This, IDropTarget __RPC_FAR *pDropTarget, IDropTarget __RPC_FAR *__RPC_FAR *ppDropTarget)
{
    // Return our IDropTarget object associated with this IDocHostUIHandler object. I don't
    // know why we don't do this via UI_QueryInterface(), but we don't.

    // NOTE: If we want/need an IDropTarget interface, then we would have had to setup our own
    // IDropTarget functions, IDropTarget VTable, and create an IDropTarget object. We'd want to put
    // a pointer to the IDropTarget object in our own custom IDocHostUIHandlerEx object (like how
    // we may add an HWND member for the use of UI_ShowContextMenu). So when we created our
    // IDocHostUIHandlerEx object, maybe we'd add a 'idrop' member to the end of it, and
    // store a pointer to our IDropTarget object there. Then we could return this pointer as so:
    //
    // *pDropTarget = ((IDocHostUIHandlerEx FAR *)This)->idrop;
    // return(S_OK);

    // But for our purposes, we don't need an IDropTarget object, so we'll tell whomever is calling
    // us that we don't have one.
    return S_FALSE;
}

// Called by the browser when it wants a pointer to our IDispatch object. This object allows us to expose
// our own automation interface (ie, our own COM objects) to other entities that are running within the
// context of the browser so they can call our functions if they want. An example could be a javascript
// running in the URL we display could call our IDispatch functions. We'd write them so that any args passed
// to them would use the generic datatypes like a BSTR for utmost flexibility.
static HRESULT STDMETHODCALLTYPE UI_GetExternal(IDocHostUIHandler *This, IDispatch **ppDispatch)
{
    // Return our IDispatch object associated with this IDocHostUIHandler object. I don't
    // know why we don't do this via UI_QueryInterface(), but we don't.

    // NOTE: If we want/need an IDispatch interface, then we would have had to setup our own
    // IDispatch functions, IDispatch VTable, and create an IDispatch object. We'd want to put
    // a pointer to the IDispatch object in our custom _IDocHostUIHandlerEx object (like how
    // we may add an HWND member for the use of UI_ShowContextMenu). So when we defined our
    // _IDocHostUIHandlerEx object, maybe we'd add a 'idispatch' member to the end of it, and
    // store a pointer to our IDispatch object there. Then we could return this pointer as so:
    //
    // *ppDispatch = ((_IDocHostUIHandlerEx FAR *)This)->idispatch;
    // return(S_OK);

    // But for our purposes, we don't need an IDispatch object, so we'll tell whomever is calling
    // us that we don't have one. Note: We must set ppDispatch to 0 if we don't return our own
    // IDispatch object.
    *ppDispatch = 0;
    return S_FALSE;
}

/* ************************* asciiToNumW() **************************
 * Converts the OLECHAR string of digits (expressed in base 10) to a
 * 32-bit DWORD.
 *
 * val =    Pointer to the nul-terminated string of digits to convert.
 *
 * RETURNS: The integer value as a DWORD.
 *
 * NOTE: Skips leading spaces before the first digit.
 */

static DWORD asciiToNumW(OLECHAR *val)
{
    OLECHAR         chr;
    DWORD           len;

    // Result is initially 0
    len = 0;

    // Skip leading spaces
    while (*val == ' ' || *val == 0x09) val++;

    // Convert next digit
    while (*val)
    {
        chr = *(val)++ - '0';
        if ((DWORD)chr > 9) break;
        len += (len + (len << 3) + chr);
    }

    return(len);
}

// Called by the browser object to give us an opportunity to modify the URL to be loaded.
static HRESULT STDMETHODCALLTYPE UI_TranslateUrl(IDocHostUIHandler *This, DWORD dwTranslate, OLECHAR *pchURLIn, OLECHAR **ppchURLOut)
{
    unsigned short  *src;
    unsigned short  *dest;
    DWORD           len;

    // Get length of URL
    src = pchURLIn;
    while ((*(src)++));
    --src;
    len = src - pchURLIn; 

    // See if the URL starts with 'app:'. We will use this as a "special link" that can be
    // placed on a web page. The URL will be in the format "app:XXX" where XXX is some
    // number. For example, maybe the following will be placed on the web page:
    //
    // <A HREF="app:1">Some special link</A>
    //
    // When the user clicks on it, we will substitute a blank page, and then send the
    // application a WM_APP message with wParam as the number after the app:. The
    // application can then do anything it wants as a result of this, for example,
    // call osWebViewLoadHTML to load some other string in memory, or whatever.
    if (len >= 4 && pchURLIn[0]==L'a' && pchURLIn[1]==L'p' && pchURLIn[2]==L'p' && pchURLIn[3]==L':')
    {
        // Allocate a new buffer to return an "about:blank" URL
        if ((dest = (OLECHAR *)CoTaskMemAlloc(12<<1)))
        {
            HWND    hwnd;

            *ppchURLOut = dest;

            // Return "about:blank"
            CopyMemory(dest, &Blank[0], 12<<1);

            // Convert the number after the "app:"
            len = asciiToNumW(pchURLIn + 4);

            // Get our host window. That was stored in our _IOleInPlaceFrameEx
            hwnd = ((_IOleInPlaceSiteEx *)((char *)This - sizeof(_IOleInPlaceSiteEx)))->frame.window;

            // Post a message to this window using WM_APP, and pass the number converted above.
            // Do not SendMessage()!. Post instead, since the browser does not like us changing
            // the URL within this here callback.
            PostMessageW(hwnd, WM_APP, (WPARAM)len, 0);

            // Tell browser that we returned a URL
            return(S_OK);
        }
    }

    // We don't need to modify the URL. Note: We need to set ppchURLOut to 0 if we don't
    // return an OLECHAR (buffer) containing a modified version of pchURLIn.
    *ppchURLOut = 0;
    return(S_FALSE);
}

// Called by the browser when it does cut/paste to the clipboard. This allows us to block certain clipboard
// formats or support additional clipboard formats.
static HRESULT STDMETHODCALLTYPE UI_FilterDataObject(IDocHostUIHandler * This, IDataObject *pDO, IDataObject **ppDORet)
{
    // Return our IDataObject object associated with this IDocHostUIHandler object. I don't
    // know why we don't do this via UI_QueryInterface(), but we don't.

    // NOTE: If we want/need an IDataObject interface, then we would have had to setup our own
    // IDataObject functions, IDataObject VTable, and create an IDataObject object. We'd want to put
    // a pointer to the IDataObject object in our custom _IDocHostUIHandlerEx object (like how
    // we may add an HWND member for the use of UI_ShowContextMenu). So when we defined our
    // _IDocHostUIHandlerEx object, maybe we'd add a 'idata' member to the end of it, and
    // store a pointer to our IDataObject object there. Then we could return this pointer as so:
    //
    // *ppDORet = ((_IDocHostUIHandlerEx FAR *)This)->idata;
    // return(S_OK);

    // But for our purposes, we don't need an IDataObject object, so we'll tell whomever is calling
    // us that we don't have one. Note: We must set ppDORet to 0 if we don't return our own
    // IDataObject object.
    *ppDORet = 0;
    return(S_FALSE);
}






////////////////////////////////////// My IOleClientSite functions  /////////////////////////////////////
// We give the browser object a pointer to our IOleClientSite object when we call OleCreate() or DoVerb().

/************************* Site_QueryInterface() *************************
 * The browser object calls this when it wants a pointer to one of our
 * IOleClientSite, IDocHostUIHandler, or IOleInPlaceSite structures. They
 * are all accessible via the _IOleClientSiteEx struct we allocated in
 * WM_CREATE and passed to DoVerb() and OleCreate().
 *
 * This =       A pointer to whatever _IOleClientSiteEx struct we passed to
 *              OleCreate() or DoVerb().
 * riid =       A GUID struct that the browser passes us to clue us as to
 *              which type of struct (object) it would like a pointer
 *              returned for.
 * ppvObject =  Where the browser wants us to return a pointer to the
 *              appropriate struct. (ie, It passes us a handle to fill in).
 *
 * RETURNS: S_OK if we return the struct, or E_NOINTERFACE if we don't have
 * the requested struct.
 */

static HRESULT STDMETHODCALLTYPE Site_QueryInterface(IOleClientSite *This, REFIID riid, void **ppvObject)
{
    // It just so happens that the first arg passed to us is our _IOleClientSiteEx struct we allocated
    // and passed to DoVerb() and OleCreate(). Nevermind that 'This' is declared is an IOleClientSite *.
    // Remember that in WM_CREATE, we allocated our own _IOleClientSiteEx struct and its first member is
    // IOleClientSite.

    // If the browser is asking us to match IID_IOleClientSite, then it wants us to return a pointer to
    // our IOleClientSite struct. Then the browser will use the VTable in that struct to call our
    // IOleClientSite functions. It will also pass this same pointer to all of our IOleClientSite
    // functions.
    //
    // Actually, we're going to lie to the browser again. We're going to return our own _IOleClientSiteEx
    // struct, and tell the browser that it's a IOleClientSite struct. It's ok. The first thing in our
    // _IOleClientSiteEx is an embedded IOleClientSite, so the browser doesn't mind. We want the browser
    // to continue passing our _IOleClientSiteEx pointer wherever it would normally pass a IOleClientSite
    // pointer.
    // 
    // The IUnknown interface uses the same VTable as the first object in our _IOleClientSiteEx
    // struct (which happens to be an IOleClientSite). So if the browser is asking us to match
    // IID_IUnknown, then we'll also return a pointer to our _IOleClientSiteEx.

    if (!memcmp(riid, &IID_IUnknown, sizeof(GUID)) || !memcmp(riid, &IID_IOleClientSite, sizeof(GUID)))
        *ppvObject = &((_IOleClientSiteEx *)This)->client;

    // If the browser is asking us to match IID_IOleInPlaceSite, then it wants us to return a pointer to
    // our IOleInPlaceSite struct. Then the browser will use the VTable in that struct to call our
    // IOleInPlaceSite functions.  It will also pass this same pointer to all of our IOleInPlaceSite
    // functions (except for Site_QueryInterface, Site_AddRef, and Site_Release. Those will always get
    // the pointer to our _IOleClientSiteEx.
    //
    // Actually, we're going to lie to the browser. We're going to return our own _IOleInPlaceSiteEx
    // struct, and tell the browser that it's a IOleInPlaceSite struct. It's ok. The first thing in
    // our _IOleInPlaceSiteEx is an embedded IOleInPlaceSite, so the browser doesn't mind. We want the
    // browser to continue passing our _IOleInPlaceSiteEx pointer wherever it would normally pass a
    // IOleInPlaceSite pointer.
    else if (!memcmp(riid, &IID_IOleInPlaceSite, sizeof(GUID)))
        *ppvObject = &((_IOleClientSiteEx *)This)->inplace;

    // If the browser is asking us to match IID_IDocHostUIHandler, then it wants us to return a pointer to
    // our IDocHostUIHandler struct. Then the browser will use the VTable in that struct to call our
    // IDocHostUIHandler functions.  It will also pass this same pointer to all of our IDocHostUIHandler
    // functions (except for Site_QueryInterface, Site_AddRef, and Site_Release. Those will always get
    // the pointer to our _IOleClientSiteEx.
    //
    // Actually, we're going to lie to the browser. We're going to return our own _IDocHostUIHandlerEx
    // struct, and tell the browser that it's a IDocHostUIHandler struct. It's ok. The first thing in
    // our _IDocHostUIHandlerEx is an embedded IDocHostUIHandler, so the browser doesn't mind. We want the
    // browser to continue passing our _IDocHostUIHandlerEx pointer wherever it would normally pass a
    // IDocHostUIHandler pointer. My, we're really playing dirty tricks on the browser here. heheh.
    else if (!memcmp(riid, &IID_IDocHostUIHandler, sizeof(GUID)))
        *ppvObject = &((_IOleClientSiteEx *)This)->ui;

    // For other types of objects the browser wants, just report that we don't have any such objects.
    // NOTE: If you want to add additional functionality to your browser hosting, you may need to
    // provide some more objects here. You'll have to investigate what the browser is asking for
    // (ie, what REFIID it is passing).
    else
    {
        *ppvObject = 0;
        return E_NOINTERFACE;
    }

    return S_OK;
}

static ULONG STDMETHODCALLTYPE Site_AddRef(IOleClientSite *This)
{
    return 1;
}

static ULONG STDMETHODCALLTYPE Site_Release(IOleClientSite *This)
{
    return 1;
}

static HRESULT STDMETHODCALLTYPE Site_SaveObject(IOleClientSite *This)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Site_GetMoniker(IOleClientSite *This, DWORD dwAssign, DWORD dwWhichMoniker, IMoniker **ppmk)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Site_GetContainer(IOleClientSite *This, LPOLECONTAINER *ppContainer)
{
    // Tell the browser that we are a simple object and don't support a container
    *ppContainer = 0;

    return E_NOINTERFACE;
}

static HRESULT STDMETHODCALLTYPE Site_ShowObject(IOleClientSite *This)
{
    return NOERROR;
}

static HRESULT STDMETHODCALLTYPE Site_OnShowWindow(IOleClientSite *This, BOOL fShow)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Site_RequestNewObjectLayout(IOleClientSite *This)
{
    return E_NOTIMPL;
}


////////////////////////////////////// My IOleInPlaceSite functions  /////////////////////////////////////
// The browser object asks us for the pointer to our IOleInPlaceSite object by calling our IOleClientSite's
// QueryInterface (ie, Site_QueryInterface) and specifying a REFIID of IID_IOleInPlaceSite.

static HRESULT STDMETHODCALLTYPE InPlace_QueryInterface(IOleInPlaceSite *This, REFIID riid, LPVOID * ppvObj)
{
    // The browser assumes that our IOleInPlaceSite object is associated with our IOleClientSite
    // object. So it is possible that the browser may call our IOleInPlaceSite's QueryInterface()
    // to ask us to return a pointer to our IOleClientSite, in the same way that the browser calls
    // our IOleClientSite's QueryInterface() to ask for a pointer to our IOleInPlaceSite.
    //
    // Rather than duplicate much of the code in IOleClientSite's QueryInterface, let's just get
    // a pointer to our _IOleClientSiteEx object, substitute it as the 'This' arg, and call our
    // our IOleClientSite's QueryInterface. Note that since our IOleInPlaceSite is embedded right
    // inside our _IOleClientSiteEx, and comes immediately after the IOleClientSite, we can employ
    // the following trickery to get the pointer to our _IOleClientSiteEx.
    return Site_QueryInterface((IOleClientSite *)((char *)This - sizeof(IOleClientSite)), riid, ppvObj);
}

static ULONG STDMETHODCALLTYPE InPlace_AddRef(IOleInPlaceSite *This)
{
    return 1;
}

static ULONG STDMETHODCALLTYPE InPlace_Release(IOleInPlaceSite *This)
{
    return 1;
}

static HRESULT STDMETHODCALLTYPE InPlace_GetWindow(IOleInPlaceSite *This, HWND * lphwnd)
{
    // Return the HWND of the window that contains this browser object. We stored that
    // HWND in our _IOleInPlaceSiteEx struct. Nevermind that the function declaration for
    // Site_GetWindow says that 'This' is an IOleInPlaceSite *. Remember that in
    // WM_CREATE, we allocated our own _IOleInPlaceSiteEx struct which
    // contained an embedded IOleInPlaceSite struct within it. And when the browser
    // called Site_QueryInterface() to get a pointer to our IOleInPlaceSite object, we
    // returned a pointer to our _IOleClientSiteEx. The browser doesn't know this. But
    // we do. That's what we're really being passed, so we can recast it and use it as
    // so here.
    *lphwnd = ((_IOleInPlaceSiteEx *)This)->frame.window;

    return S_OK;
}

static HRESULT STDMETHODCALLTYPE InPlace_ContextSensitiveHelp(IOleInPlaceSite *This, BOOL fEnterMode)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE InPlace_CanInPlaceActivate(IOleInPlaceSite *This)
{
    // Tell the browser we can in place activate
    return S_OK;
}

static HRESULT STDMETHODCALLTYPE InPlace_OnInPlaceActivate(IOleInPlaceSite *This)
{
    // Tell the browser we did it ok
    return S_OK;
}

static HRESULT STDMETHODCALLTYPE InPlace_OnUIActivate(IOleInPlaceSite *This)
{
    return(S_OK);
}

static HRESULT STDMETHODCALLTYPE InPlace_GetWindowContext(IOleInPlaceSite *This, LPOLEINPLACEFRAME *lplpFrame, LPOLEINPLACEUIWINDOW *lplpDoc, LPRECT lprcPosRect, LPRECT lprcClipRect, LPOLEINPLACEFRAMEINFO lpFrameInfo)
{
    // Give the browser the pointer to our IOleInPlaceFrame struct. We stored that pointer
    // in our _IOleInPlaceSiteEx struct. Nevermind that the function declaration for
    // Site_GetWindowContext says that 'This' is an IOleInPlaceSite *. Remember that in
    // WM_CREATE, we allocated our own _IOleInPlaceSiteEx struct which
    // contained an embedded IOleInPlaceSite struct within it. And when the browser
    // called Site_QueryInterface() to get a pointer to our IOleInPlaceSite object, we
    // returned a pointer to our _IOleClientSiteEx. The browser doesn't know this. But
    // we do. That's what we're really being passed, so we can recast it and use it as
    // so here.
    //
    // Actually, we're giving the browser a pointer to our own _IOleInPlaceSiteEx struct,
    // but telling the browser that it's a IOleInPlaceSite struct. No problem. Our
    // _IOleInPlaceSiteEx starts with an embedded IOleInPlaceSite, so the browser is
    // cool with it. And we want the browser to pass a pointer to this _IOleInPlaceSiteEx
    // wherever it would pass a IOleInPlaceSite struct to our IOleInPlaceSite functions.
    *lplpFrame = (LPOLEINPLACEFRAME)&((_IOleInPlaceSiteEx *)This)->frame;

    // We have no OLEINPLACEUIWINDOW
    *lplpDoc = 0;

    // Fill in some other info for the browser
    lpFrameInfo->fMDIApp = FALSE;
    lpFrameInfo->hwndFrame = ((_IOleInPlaceFrameEx *)*lplpFrame)->window;
    lpFrameInfo->haccel = 0;
    lpFrameInfo->cAccelEntries = 0;

    // Give the browser the dimensions of where it can draw. We give it our entire window to fill.
    // We do this in InPlace_OnPosRectChange() which is called right when a window is first
    // created anyway, so no need to duplicate it here.
//  GetClientRect(lpFrameInfo->hwndFrame, lprcPosRect);
//  GetClientRect(lpFrameInfo->hwndFrame, lprcClipRect);

    return S_OK;
}

static HRESULT STDMETHODCALLTYPE InPlace_Scroll(IOleInPlaceSite *This, SIZE scrollExtent)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE InPlace_OnUIDeactivate(IOleInPlaceSite *This, BOOL fUndoable)
{
    return(S_OK);
}

static HRESULT STDMETHODCALLTYPE InPlace_OnInPlaceDeactivate(IOleInPlaceSite *This)
{
    return(S_OK);
}

static HRESULT STDMETHODCALLTYPE InPlace_DiscardUndoState(IOleInPlaceSite *This)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE InPlace_DeactivateAndUndo(IOleInPlaceSite *This)
{
    return E_NOTIMPL;
}

// Called when the position of the browser object is changed, such as when we call the IWebBrowser2's put_Width(),
// put_Height(), put_Left(), or put_Right().
static HRESULT STDMETHODCALLTYPE InPlace_OnPosRectChange(IOleInPlaceSite *This, LPCRECT lprcPosRect)
{
    IOleInPlaceObject   *inplace;
    _IOleClientSiteEx *_iOleClientSiteEx = (_IOleClientSiteEx*) ((char *)This - sizeof(IOleClientSite));

    if (!_iOleClientSiteEx->browserObject->lpVtbl->QueryInterface(_iOleClientSiteEx->browserObject, &IID_IOleInPlaceObject, (void **)&inplace))
    {
        // Give the browser the dimensions of where it can draw.
        inplace->lpVtbl->SetObjectRects(inplace, lprcPosRect, lprcPosRect);
    }

    return S_OK;
}


////////////////////////////////////// My IOleInPlaceFrame functions  /////////////////////////////////////////

static HRESULT STDMETHODCALLTYPE Frame_QueryInterface(IOleInPlaceFrame *This, REFIID riid, LPVOID *ppvObj)
{
    return E_NOTIMPL;
}

static ULONG STDMETHODCALLTYPE Frame_AddRef(IOleInPlaceFrame *This)
{
    return(1);
}

static ULONG STDMETHODCALLTYPE Frame_Release(IOleInPlaceFrame *This)
{
    return(1);
}

static HRESULT STDMETHODCALLTYPE Frame_GetWindow(IOleInPlaceFrame *This, HWND *lphwnd)
{
    // Give the browser the HWND to our window that contains the browser object. We
    // stored that HWND in our IOleInPlaceFrame struct. Nevermind that the function
    // declaration for Frame_GetWindow says that 'This' is an IOleInPlaceFrame *. Remember
    // that in WM_CREATE, we allocated our own IOleInPlaceFrameEx struct which
    // contained an embedded IOleInPlaceFrame struct within it. And then we lied when
    // Site_GetWindowContext() returned that IOleInPlaceFrameEx. So that's what the
    // browser is passing us. It doesn't know that. But we do. So we can recast it and
    // use it as so here.
    *lphwnd = ((_IOleInPlaceFrameEx *)This)->window;
    return S_OK;
}

static HRESULT STDMETHODCALLTYPE Frame_ContextSensitiveHelp(IOleInPlaceFrame *This, BOOL fEnterMode)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Frame_GetBorder(IOleInPlaceFrame *This, LPRECT lprectBorder)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Frame_RequestBorderSpace(IOleInPlaceFrame * This, LPCBORDERWIDTHS pborderwidths)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Frame_SetBorderSpace(IOleInPlaceFrame * This, LPCBORDERWIDTHS pborderwidths)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Frame_SetActiveObject(IOleInPlaceFrame * This, IOleInPlaceActiveObject *pActiveObject, LPCOLESTR pszObjName)
{
    return S_OK;
}

static HRESULT STDMETHODCALLTYPE Frame_InsertMenus(IOleInPlaceFrame * This, HMENU hmenuShared, LPOLEMENUGROUPWIDTHS lpMenuWidths)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Frame_SetMenu(IOleInPlaceFrame * This, HMENU hmenuShared, HOLEMENU holemenu, HWND hwndActiveObject)
{
    return S_OK;
}

static HRESULT STDMETHODCALLTYPE Frame_RemoveMenus(IOleInPlaceFrame * This, HMENU hmenuShared)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Frame_SetStatusText(IOleInPlaceFrame * This, LPCOLESTR pszStatusText)
{
    return S_OK;
}

static HRESULT STDMETHODCALLTYPE Frame_EnableModeless(IOleInPlaceFrame * This, BOOL fEnable)
{
    return S_OK;
}

static HRESULT STDMETHODCALLTYPE Frame_TranslateAccelerator(IOleInPlaceFrame * This, LPMSG lpmsg, WORD wID)
{
    return E_NOTIMPL;
}






//////////////////////////////////// My IDispatch functions  //////////////////////////////////////
// The browser uses our IDispatch to give feedback when certain actions occur on the web page.

static HRESULT STDMETHODCALLTYPE Dispatch_QueryInterface(IDispatch * This, REFIID riid, void **ppvObject)
{
    *ppvObject = 0;

    if (!memcmp(riid, &IID_IUnknown, sizeof(GUID)) || !memcmp(riid, &IID_IDispatch, sizeof(GUID)))
    {
        *ppvObject = (void *)This;

        // Increment its usage count. The caller will be expected to call our
        // IDispatch's Release() (ie, Dispatch_Release) when it's done with
        // our IDispatch.
        Dispatch_AddRef(This);

        return(S_OK);
    }

    *ppvObject = 0;
    return(E_NOINTERFACE);
}

static ULONG STDMETHODCALLTYPE Dispatch_AddRef(IDispatch *This)
{
    return InterlockedIncrement(&((_IDispatchEx *)This)->refCount);
}

static ULONG STDMETHODCALLTYPE Dispatch_Release(IDispatch *This)
{
    if (InterlockedDecrement( &((_IDispatchEx *)This)->refCount) == 0)
    {
        /* If you uncomment the following line you should get one message
         * when the document unloads for each successful call to
         * CreateEventHandler. If not, check you are setting all events
         * (that you set), to null or detaching them.
         */
        // OutputDebugString("One event handler destroyed");

        GlobalFree(((char *)This - ((_IDispatchEx *)This)->extraSize));
        return(0);
    }

    return ((_IDispatchEx *)This)->refCount;
}

static HRESULT STDMETHODCALLTYPE Dispatch_GetTypeInfoCount(IDispatch *This, unsigned int *pctinfo)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Dispatch_GetTypeInfo(IDispatch *This, unsigned int iTInfo, LCID lcid, ITypeInfo **ppTInfo)
{
    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Dispatch_GetIDsOfNames(IDispatch *This, REFIID riid, OLECHAR ** rgszNames, unsigned int cNames, LCID lcid, DISPID * rgDispId)
{
    return S_OK;
}

static void webDetach(_IDispatchEx *lpDispatchEx)
{
    IHTMLWindow3    *htmlWindow3;

    // Get the IHTMLWindow3 and call its detachEvent() to disconnect our _IDispatchEx
    // from the element on the web page.
    if (!lpDispatchEx->htmlWindow2->lpVtbl->QueryInterface(lpDispatchEx->htmlWindow2, (GUID *)&_IID_IHTMLWindow3[0], (void **)&htmlWindow3) && htmlWindow3)
    {
        htmlWindow3->lpVtbl->detachEvent(htmlWindow3, OnBeforeOnLoad, (LPDISPATCH)lpDispatchEx);
        htmlWindow3->lpVtbl->Release(htmlWindow3);
    }

    // Release any object that was originally passed to CreateEventHandler().
    if (lpDispatchEx->object) lpDispatchEx->object->lpVtbl->Release(lpDispatchEx->object);

    // We don't need the IHTMLWindow2 any more (that we got in CreateEventHandler).
    lpDispatchEx->htmlWindow2->lpVtbl->Release(lpDispatchEx->htmlWindow2);
}

static HRESULT STDMETHODCALLTYPE Dispatch_Invoke(IDispatch *This, DISPID dispIdMember, REFIID riid, LCID lcid, WORD wFlags, DISPPARAMS * pDispParams, VARIANT *pVarResult, EXCEPINFO *pExcepInfo, unsigned int *puArgErr)
{
    WEBPARAMS       webParams;
    BSTR            strType;

    // Get the IHTMLEventObj from the associated window.
    if (!((_IDispatchEx *)This)->htmlWindow2->lpVtbl->get_event(((_IDispatchEx *)This)->htmlWindow2, &webParams.htmlEvent) && webParams.htmlEvent)
    {
        // Get the event's type (ie, a BSTR) by calling the IHTMLEventObj's get_type().
        webParams.htmlEvent->lpVtbl->get_type(webParams.htmlEvent, &strType);
        if (strType)
        {
            // Set the WEBPARAMS.NMHDR struct's hwndFrom to the window hosting the browser,
            // and set idFrom to 0 to let him know that this is a message that was sent
            // as a result of an action occuring on the web page (that the app asked to be
            // informed of).
            webParams.nmhdr.hwndFrom = ((_IDispatchEx *)This)->hwnd;

            // Is the type "beforeunload"? If so, set WEBPARAMS.NMHDR struct's "code" to 0, otherwise 1.
            if (!(webParams.nmhdr.code = lstrcmpW(strType, &BeforeUnload[0])))
            {
                // This is a "beforeunload" event. NOTE: This should be the last event before our
                // _IDispatchEx is destroyed.

                // If the id number is negative, then this is the app's way of telling us that it
                // expects us to stuff the "eventStr" arg (passed to CreateEventHandler) into the
                // WEBPARAMS->eventStr.
                if (((_IDispatchEx *)This)->id < 0)
                {   
user:               webParams.eventStr = (LPCTSTR)((_IDispatchEx *)This)->userdata;
                }

                // We can free the BSTR we got above since we no longer need it.
                goto freestr;
            }

            // It's some other type of event. Set the WEBPARAMS->eventStr so he can do a lstrcmp()
            // to see what event happened.
            else
            {
                // Let app know that this is not a "beforeunload" event.
                webParams.nmhdr.code = 1;

                // If the app wants us to set the event type string into the WEBPARAMS, then get that
                // string as UNICODE or ANSI -- whichever is appropriate for the app window.
                if (((_IDispatchEx *)This)->id < 0) goto user;
                if (!IsWindowUnicode(webParams.nmhdr.hwndFrom))
                {
                    // For ANSI, we need to convert the BSTR to an ANSI string, and then we no longer
                    // need the BSTR.
                    webParams.nmhdr.idFrom = WideCharToMultiByte(CP_ACP, 0, (WCHAR *)strType, -1, 0, 0, 0, 0);
                    if (!(webParams.eventStr = GlobalAlloc(GMEM_FIXED, sizeof(char) * webParams.nmhdr.idFrom))) goto bad;
                    WideCharToMultiByte(CP_ACP, 0, (WCHAR *)strType, -1, (char *)webParams.eventStr, webParams.nmhdr.idFrom, 0, 0);
freestr:            SysFreeString(strType);
                    strType = 0;
                }

                // For UNICODE, we can use the BSTR as is. We can't free the BSTR yet.
                else
                    webParams.eventStr = (LPCTSTR)strType;
            }
    
            // Send a WM_NOTIFY message to the window with the _IDispatchEx as WPARAM, and
            // the WEBPARAMS as LPARAM.
            webParams.nmhdr.idFrom = 0;
            SendMessage(webParams.nmhdr.hwndFrom, WM_NOTIFY, (WPARAM)This, (LPARAM)&webParams);

            // Free anything allocated or gotten above
bad:        if (strType) SysFreeString(strType);
            else if (webParams.eventStr && ((_IDispatchEx *)This)->id >= 0) GlobalFree((void *)webParams.eventStr);

            // If this was the "beforeunload" event, detach this IDispatch from that event for its web page element.
            // This should also cause the IE engine to call this IDispatch's Dispatch_Release().
            if (!webParams.nmhdr.code) webDetach((_IDispatchEx *)This);
        }

        // Release the IHTMLEventObj.
        webParams.htmlEvent->lpVtbl->Release(webParams.htmlEvent);
    }

    return S_OK;
}



/************************ CreateWebEvtHandler() *********************
 * Creates an event handler, to be used to "attach to" some events that
 * happpen with an element on the web page.
 *
 * hwnd - The window where messages will be sent when the event happens.
 *
 * htmlDoc2 - Pointer to an IHTMLDocument2 object. Objects that use
 *            the resulting event handler must be associated with this
 *            (ie. either its parent window, itself or a child element).
 *
 * extraData -  sizeof() any application defined struct you wish
 *              prepended to the returned IDispatch. You can use
 *              GetWebExtraData() to fetch a pointer to this struct.
 *              0 if no extra data needed.
 *
 * id -     Any numeric value of your choosing to be stored in the
 *          _IDispatchEx's "id" member. If a negative value, then the
 *          WEBPARAMS->eventStr will be set to the passed USERDATA
 *          instead of an event string.
 *
 * obj -    A pointer to any other object to be stored in the _IDispatchEx's
 *          "object" member. 0 if none.
 *
 * userdata -   If not zero, then this will be stored in the _IDispatchEx's
 *              "userdata" member.
 *
 * attachObj -  If not zero, then "userdata" is considered to be a BSTR of
 *              some event type to attach to, and "attachObj" is the
 *              
 *
 * RETURNS: Pointer to the IDispatch if success, or 0 if an error.
 *
 * NOTE: "elem" will automatically be Release()'ed by this DLL when the
 * _IDispatchEx is destroyed. It is also Release()'ed if this call fails.
 */

static IDispatch * WINAPI CreateWebEvtHandler(HWND hwnd, IHTMLDocument2 * htmlDoc2, DWORD extraData, long id, IUnknown *obj, void *userdata)
{
    _IDispatchEx *  lpDispatchEx;
    IHTMLWindow2 *  htmlWindow2;
    IHTMLWindow3 *  htmlWindow3;
    VARIANT         varDisp;

    // Get the IHTMLWindow2. Our IDispatch's Invoke() will need it to cleanup.
    if (!htmlDoc2->lpVtbl->get_parentWindow(htmlDoc2, &htmlWindow2) && htmlWindow2)
    {
        VariantInit(&varDisp);

        // If he didn't pass any userdata, then we don't need the extra "userdata" member
        // on the IDispatch.
        XV2(varDisp).vt = 0;
        if (!userdata && id >= 0) XV2(varDisp).vt -= sizeof(void *);

        // Create an IDispatch object (actually we create one of our own _IDispatchEx objects)
        // which we'll use to monitor "events" that occur to an element on a web page.
        // IE's engine will call our IDispatch's Invoke() function when it wants to inform
        // us that a specific event has occurred.
        if ((lpDispatchEx = (_IDispatchEx *)GlobalAlloc(GMEM_FIXED, sizeof(_IDispatchEx) + extraData + XV2(varDisp).vt)))
        {
            // Clear out the extradata area in case the caller wants that.
            ZeroMemory(lpDispatchEx, extraData);

            // Skip past the extradata.
            lpDispatchEx = (_IDispatchEx *)((char *)lpDispatchEx + extraData);  

            // Fill in our _IDispatchEx with its VTable, and the args passed to us by the caller
            lpDispatchEx->dispatchObj.lpVtbl = &MyIDispatchVtbl;
            lpDispatchEx->hwnd = hwnd;
            lpDispatchEx->htmlWindow2 = htmlWindow2;
            lpDispatchEx->id = (short)id;
            lpDispatchEx->extraSize = (unsigned short)extraData;
            lpDispatchEx->object = obj;
            if (userdata) lpDispatchEx->userdata = userdata;

            // No one has yet called its Dispatch_AddRef(). That won't happen until we
            // attach some event to it, such as below.
            lpDispatchEx->refCount = 0;

            // Now we attach our IDispatch to the "beforeunload" event so that our IDispatch's
            // Invoke() is called when the browser fires off this event.

            // We need to get the IHTMLWindow3 object (so we can call its attachEvent() and pass it
            // our IDispatch wrapped in a VARIANT).
            if (!htmlWindow2->lpVtbl->QueryInterface(htmlWindow2, (GUID *)&_IID_IHTMLWindow3[0], (void **)&htmlWindow3) && htmlWindow3)
            {
				VARIANT_BOOL result;
                if (!htmlWindow3->lpVtbl->attachEvent(htmlWindow3, OnBeforeOnLoad, (LPDISPATCH)lpDispatchEx, &result))
                {
                    // Release() the IHTMLWindow3 object now that we called its attachEvent().
                    htmlWindow3->lpVtbl->Release(htmlWindow3);

                    // Return the IDispatch, so the app can use it to attach some other events to the
                    // same element on the web page.
                    //
                    // NOTE: We don't Release() the IHTMLWindow2 object. We stored that pointer
                    // in our _IDispatchEx and its Invoke() needs it. webDetach() will
                    // Release() it.
                    return (IDispatch *)lpDispatchEx;
                }

                // An error. Free all stuff above.
bad:            htmlWindow3->lpVtbl->Release(htmlWindow3);
            }

            GlobalFree(((char *)lpDispatchEx - lpDispatchEx->extraSize));
        }

        htmlWindow2->lpVtbl->Release(htmlWindow2);
    }

    // Release whatever the app passed, so it doesn't need to do that in case of an error.
    if (obj) obj->lpVtbl->Release(obj);

    // FAILURE.
    return 0;
}





/********************** FreeWebEvtHandler() *********************
 * Called to detach our _IDispatchEx from the "onbeforeunload" event
 * (ie, it was attached in CreateWebEvtHandler(). Also frees the
 * IHTMLWindow2 that CreateWebEvtHandler() got.
 *
 * NOTE: FreeWebEvtHandler() must be called only if
 * CreateWebEvtHandler() succeeds, and you wish to later manually free
 * that event handler before loading a web page. Typically, this needs
 * to be done only if you have a failure while setting up for an event
 * handler.
 *
 * If the caller has other events still attached to this same
 * _IDispatchEx, then it won't really be expunged from memory. So,
 * the caller should ensure that it detaches all events it attached.
 */

static void WINAPI FreeWebEvtHandler(IDispatch *lpDispatch)
{
    webDetach((_IDispatchEx *)lpDispatch);
}





/************************** GetWebSrcElement() **********************
 * Retrieves the IHTMLElement of some event object. The returned
 * IHTMLElement's QueryInterface() function can be called to
 * obtain other objects such as IHTMLInputElement, etc.
 * 
 * Returns: Pointer to the IHTMLElement object, or 0 if an error.
 */

static IHTMLElement * WINAPI GetWebSrcElement(IHTMLEventObj *htmlEvent)
{
    IHTMLElement *htmlElement;

    htmlElement = 0;
    htmlEvent->lpVtbl->get_srcElement(htmlEvent, &htmlElement);
    return(htmlElement);
}





/************************** SetWebReturnValue() **********************
 * Sets the return value (to some IE Object function) to either
 * true or false. Some events (such as onsubmit) can return false
 * to be cancelled.
 */

static HRESULT WINAPI SetWebReturnValue(IHTMLEventObj *htmlEvent, BOOL returnVal)
{
    VARIANT varResult;

    XV2(varResult).vt = VT_BOOL;

    if (returnVal)
		XV3(varResult).boolVal = (VARIANT_BOOL)-1;
    else
		XV3(varResult).boolVal = (VARIANT_BOOL)0;

    return(htmlEvent->lpVtbl->put_returnValue(htmlEvent, varResult));
}





/************************** GetWebPtrs() **********************
 * Fetches pointers to the IWebBrowser2 and/or the IHTMLDocument2
 * objects for the browser object associated with the passed window.
 *
 * hwnd = Handle of the window containing the browser control.
 *
 * webBrowser2Result =  Where to store a pointer to the IWebBrowser2
 *                      object, or null if not desired.
 *
 * htmlDoc2Result =     Where to store a pointer to the IHTMLDocument2
 *                      object, or null if not desired.
 *
 * RETURNS: S_OK (0) for success and E_FAIL for failure.
 *
 * Note: It is the responsibility of the caller to Release() the
 * objects.
 *
 * Note: This will fail to return a IHTMLDocument2 interface in the
 * circumstance when a document is being loaded in the browser and its
 * readystate is below READYSTATE_LOADED(2). To get around this, always
 * use WaitOnReadyState before requesting the document interface.
 */

static HRESULT WINAPI GetWebPtrs(HWND hwnd, IWebBrowser2 **webBrowser2Result, IHTMLDocument2 **htmlDoc2Result)
{
    IOleObject      *browserObject;
    IWebBrowser2    *webBrowser2;

    // Make sure he supplied at least one of the return handles. If not, we
    // have nothing to do here
    if (webBrowser2Result || htmlDoc2Result)
    {
        // Make sure he supplied a window
        if (// Get the browser object stored in the window's USERDATA member
            !(browserObject = ((_IOleClientSiteEx *)GetWindowLongPtrW(hwnd, GWLP_USERDATA))->browserObject) ||

            // Get the IWebBrowser2 object embedded within the browser object
            browserObject->lpVtbl->QueryInterface(browserObject, &IID_IWebBrowser2, (void **)&webBrowser2))
        {
            goto fail;
        }

        // Now the pointer to our IWebBrowser2 object is in 'webBrowser2', and so its VTable is
        // webBrowser2->lpVtbl.

        // Does the caller want the IHTMLDocument2 object for the browser?
        if (htmlDoc2Result)
        {
            LPDISPATCH      lpDispatch;

            // Set his handle to NULL initially
            *htmlDoc2Result = NULL;
            lpDispatch = NULL;

            // First, we need the IDispatch object
            webBrowser2->lpVtbl->get_Document(webBrowser2, &lpDispatch);
            if (lpDispatch)
            {
                // Get the IHTMLDocument2 object embedded within the IDispatch object
                lpDispatch->lpVtbl->QueryInterface(lpDispatch, &IID_IHTMLDocument2, (void **)htmlDoc2Result);

                // Release the IDispatch object now that we have the IHTMLDocument2
                lpDispatch->lpVtbl->Release(lpDispatch);
            }

            // If we failed to get IHTMLDocument2, then free the IWebBrowser2 and
            // return an error to the caller
            if (!(*htmlDoc2Result))
            {
                webBrowser2->lpVtbl->Release(webBrowser2);
fail:           return E_FAIL;
            }
        }

        // If the caller wants the IWebBrowser2 returned, store it in his supplied handle.
        // Note: We do not Release it here. The caller must do that when he is done with it
        if (webBrowser2Result)
            *webBrowser2Result = webBrowser2;

        // If he doesn't want it returned, we need to release it here
        else
            webBrowser2->lpVtbl->Release(webBrowser2);
    }

    return S_OK;
}



/*************************** GetWebElement() **************************
 * Retrieves a pointer to the IHTMLElement object for the specified
 * element on a web page (such as a FORM).
 *
 * hwnd =   Handle of the window hosting the browser.
 *
 * htmlDoc2  = A pointer to the IHTMLDocument2 object for the web page.
 *          Could be 0 if this ptr has not yet been gotten.
 *
 * name =   The name of an element specified by the HTML attributes name
 *          or id.
 *
 * nIndex = A zero based index that specifies which element to return
 *          if there is more than element with the name specified by
 *          name. If not, pass 0.
 *
 * RETURNS: A pointer to the IHTMLElement object or 0 if a failure.
 *
 * NOTE: The QueryInterface() of the returned IHTMLElement can be used
 * to get other objects associated with that element on the web page,
 * such as IHTMLInputElement.
 */

static IHTMLElement * WINAPI GetWebElement(HWND hwnd, IHTMLDocument2 *htmlDoc2, const WCHAR *name, INT nIndex)
{
    IHTMLElementCollection *    htmlCollection;
    IHTMLElement *              htmlElem;
    LPDISPATCH                  lpDispatch;

    lpDispatch = NULL;
    htmlElem = NULL;

    // Get the browser's IHTMLDocument2 object if it wasn't passed
    if (htmlDoc2 || !GetWebPtrs(hwnd, NULL, &htmlDoc2))
    {
        // Get the IHTMLElementCollection object. We need this to get the IDispatch
        // object for the element the caller wants on the web page. And from that
        // IDispatch, we get the IHTMLElement object. Really roundabout, ain't it?
        // That's COM
        if (!htmlDoc2->lpVtbl->get_all(htmlDoc2, &htmlCollection) && htmlCollection)
        {
            VARIANT         varName;
            VARIANT         varIndex;

            // Get the IDispatch for that element. We need to call the IHTMLElementCollection
            // object's item() function, passing it the name of the element. Note that we
            // have to pass the element name as a BSTR stuffed into a VARIENT struct. And
            // we also need to stuff the index into a VARIENT struct too.
            VariantInit(&varName);
            XV2(varName).vt = VT_BSTR;
            if ((XV3(varName).bstrVal = SysAllocString(name)))
            {
                VariantInit(&varIndex);
                XV2(varIndex).vt = VT_I4;
                XV3(varIndex).lVal = nIndex;

                htmlCollection->lpVtbl->item(htmlCollection, varName, varIndex, &lpDispatch);

                // We don't need the VARIENTs anymore. This frees the string that SysAllocString() gave us
                VariantClear(&varName);
                VariantClear(&varIndex);
            }

            // Release the IHTMLElementCollection now that we're done with it.
            htmlCollection->lpVtbl->Release(htmlCollection);
            
            // Did we get the IDispatch for that element?
            if (lpDispatch)
            {
                // We can finally get the IHTMLElement object for the desired object.
                lpDispatch->lpVtbl->QueryInterface(lpDispatch, &IID_IHTMLElement, (void **)&htmlElem);

                // Release the IDispatch now that we got the IHTMLElement.
                lpDispatch->lpVtbl->Release(lpDispatch);
            }
        }
    }

    // Return the IHTMLElement ptr.
    return(htmlElem);
}





/************************** doEvents() ***********************
 * Pumps available messages in the thread's message queue.
 *
 * Note: Any windows may be destroyed within doEvents, so if a
 * caller has a handle to any window, that handle should be
 * checked with IsWindow() upon return.
 */

static void doEvents(HWND hwnd)
{
    MSG     msg;

    while (PeekMessage(&msg, hwnd, 0, 0, PM_REMOVE))
    {
        TranslateMessage(&msg); 
        DispatchMessage(&msg);
    }
}





/************************** WaitOnReadyState() **********************
 * Waits for a page (that is currently loading into the browser (for
 * the specified window) to be in specified state.
 *
 * hwnd =   Handle to the window hosting the browser object.
 *
 * rs =     The desired readystate to wait for. A list of ready states can
 *          be found at:
 * http://msdn.microsoft.com/library/default.asp?url=/workshop/browser/webbrowser/reference/enums/readystate_enum.asp
 *
 * timeout = How long to wait for the state to match 'rs'. 0 for no wait.
 *
 * webBrowser2 =    A pointer to the IWebBrowser2 for the browser control,
 *                  or null if not supplied.
 *
 * RETURNS: WORS_SUCCESS (0) if the loading page has achieved the specified
 * state by the time WaitOnReadyState returns, WORS_TIMEOUT (-1) for timeout
 * and WORS_DESTROYED (-2) if the window was destroyed or a webBrowser
 * object could not be obtained from the window.
 */

static HRESULT WINAPI WaitOnReadyState(HWND hwnd, READYSTATE rs, DWORD timeout, IWebBrowser2 *webBrowser2)
{
    READYSTATE      rsi;
    DWORD           dwStart;
    unsigned char   releaseOnComplete;

    // If the caller didn't supply the IWebBrowser2, then we need to get it (and
    // release it when we're done)
    releaseOnComplete = 0;
    if (!webBrowser2)
    {
        if (GetWebPtrs(hwnd, &webBrowser2, NULL)) goto destroyed;
        releaseOnComplete = 1;
    }

    // Get the current ready state of the loading page
    webBrowser2->lpVtbl->get_ReadyState(webBrowser2, &rsi);

    // Is the current ready state at least as high as the caller needs?
    if (rsi >= rs)
    {
        // Yes, it is. Tell the caller that the page is in a state where
        // he can proceed with whatever he wants to do.
yes:    rs = WORS_SUCCESS;

        // If we got the IWebBrowser2 ourselves above, release it now.
out:    if (releaseOnComplete) webBrowser2->lpVtbl->Release(webBrowser2);

        return((HRESULT)rs);
    }

    // Ok, the loading page is not yet in the state that the caller
    // requires. We need to timeout. We can't just Sleep() for the
    // specified amount of time. Why? Because a page will not load
    // unless we are emptying out certain messages in our thread's
    // message queue. So we need to at least call doEvents() periodically
    // while we are waiting for the ready state to be achieved.
    dwStart = GetTickCount();
    do
    {
        // Empty out messages in the message queue.
        doEvents(hwnd);

        // Make sure our window with the browser object wasn't closed down in while processing messages.
        if (!IsWindow(hwnd))
        {
            // Oops! It was. Get out of here with WORS_DESTROYED.
destroyed:  rs = WORS_DESTROYED;
            goto out;
        }

        // Is the current ready state at least as high as the caller needs?
        webBrowser2->lpVtbl->get_ReadyState(webBrowser2, &rsi);
        if (rsi >= rs) goto yes;

        // We may need a sleep here on Win9x/Me systems to avoid a system hang.
        Sleep(10);

        // Did we timeout yet?
    } while (!timeout || (GetTickCount() - dwStart) <= timeout);

    // We timed out before the page achieved the desired ready state.
    rs = WORS_TIMEOUT;
    goto out;
}

/******************************* DoPageAction() **************************
 * Implements the functionality of a "Back". "Forward", "Home", "Search",
 * "Refresh", or "Stop" button.
 *
 * hwnd =       Handle to the window hosting the browser object.
 * action =     One of the following:
 *              0 = Move back to the previously viewed web page.
 *              1 = Move forward to the previously viewed web page.
 *              2 = Move to the home page.
 *              3 = Search.
 *              4 = Refresh the page.
 *              5 = Stop the currently loading page.
 */

static void WINAPI DoPageAction(HWND hwnd, DWORD action)
{   
    IWebBrowser2    *webBrowser2;

    // Get the browser's IWebBrowser2 object.
    if (!GetWebPtrs(hwnd, &webBrowser2, NULL))
    {
        // Ok, now the pointer to our IWebBrowser2 object is in 'webBrowser2', and so its VTable is
        // webBrowser2->lpVtbl.

        // Call the desired function
        switch (action)
        {
            case WEBPAGE_GOBACK:
            {
                // Call the IWebBrowser2 object's GoBack function.
                webBrowser2->lpVtbl->GoBack(webBrowser2);
                break;
            }

            case WEBPAGE_GOFORWARD:
            {
                // Call the IWebBrowser2 object's GoForward function.
                webBrowser2->lpVtbl->GoForward(webBrowser2);
                break;
            }

            case WEBPAGE_GOHOME:
            {
                // Call the IWebBrowser2 object's GoHome function.
                webBrowser2->lpVtbl->GoHome(webBrowser2);
                break;
            }

            case WEBPAGE_SEARCH:
            {
                // Call the IWebBrowser2 object's GoSearch function.
                webBrowser2->lpVtbl->GoSearch(webBrowser2);
                break;
            }

            case WEBPAGE_REFRESH:
            {
                // Call the IWebBrowser2 object's Refresh function.
                webBrowser2->lpVtbl->Refresh(webBrowser2);
            }

            case WEBPAGE_STOP:
            {
                // Call the IWebBrowser2 object's Stop function.
                webBrowser2->lpVtbl->Stop(webBrowser2);
            }
        }

        // Release the IWebBrowser2 object.
        webBrowser2->lpVtbl->Release(webBrowser2);
    }
}


LRESULT CALLBACK HWebViewFunction(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg) {
	case WM_SIZE:
		{
			IWebBrowser2    *webBrowser2;

			if (!GetWebPtrs(hwnd, &webBrowser2, NULL)) {
				webBrowser2->lpVtbl->put_Width(webBrowser2, LOWORD(lParam));
				webBrowser2->lpVtbl->put_Height(webBrowser2, HIWORD(lParam));
				webBrowser2->lpVtbl->Release(webBrowser2);
			}
			return 0;
        }

	case WM_CREATE:
		{
			IWebBrowser2 *webBrowser2;

			// Get a pointer to the browser object and lock it down (so it doesn't "disappear" while we're using
			// it in this program). We do this by calling the OS function CoCreateInstance().
			if (!CoCreateInstance(&CLSID_WebBrowser, 0, CLSCTX_INPROC, &IID_IWebBrowser2, (void **)&webBrowser2)) {
				_IOleClientSiteEx  *_iOleClientSiteEx;

				if (!(_iOleClientSiteEx = (_IOleClientSiteEx *)GlobalAlloc(GMEM_FIXED, sizeof(_IOleClientSiteEx))))
					return -1;

				// Initialize our IOleClientSite object with a pointer to our IOleClientSite VTable.
				_iOleClientSiteEx->client.lpVtbl = &MyIOleClientSiteTable;

				// Initialize our IOleInPlaceSite object with a pointer to our IOleInPlaceSite VTable.
				_iOleClientSiteEx->inplace.inplace.lpVtbl = &MyIOleInPlaceSiteTable;

				// Initialize our IOleInPlaceFrame object with a pointer to our IOleInPlaceFrame VTable.
				_iOleClientSiteEx->inplace.frame.frame.lpVtbl = &MyIOleInPlaceFrameTable;

				// Save our HWND (in the IOleInPlaceFrame object) so our IOleInPlaceFrame functions can retrieve it.
				_iOleClientSiteEx->inplace.frame.window = hwnd;

				// Initialize our IDocHostUIHandler object with a pointer to our IDocHostUIHandler VTable.
				_iOleClientSiteEx->ui.ui.lpVtbl = &MyIDocHostUIHandlerTable;

				_iOleClientSiteEx->browserObject = NULL;

				// We need to get a pointer to IWebBrowser2's IOleObject child object
				webBrowser2->lpVtbl->QueryInterface(webBrowser2, &IID_IOleObject, (void**)&_iOleClientSiteEx->browserObject);

				if (_iOleClientSiteEx->browserObject) {
					SetWindowLongPtrW(hwnd, GWLP_USERDATA, (LONG_PTR)_iOleClientSiteEx);

					// Give the browser a pointer to my IOleClientSite object.
					if (!_iOleClientSiteEx->browserObject->lpVtbl->SetClientSite(_iOleClientSiteEx->browserObject, &_iOleClientSiteEx->client)) {
						RECT          rect;

						// Set the display area of our browser control the same as our window's size
						// and actually put the browser object into our window.
						GetClientRect(hwnd, &rect);
						if (!_iOleClientSiteEx->browserObject->lpVtbl->DoVerb(_iOleClientSiteEx->browserObject, OLEIVERB_INPLACEACTIVATE, 0, &_iOleClientSiteEx->client, 0, hwnd, &rect)) {
							// Let's call several functions in the IWebBrowser2 object to position the browser display area in our window.
							webBrowser2->lpVtbl->put_Left(webBrowser2, 0);
							webBrowser2->lpVtbl->put_Top(webBrowser2, 0);
							webBrowser2->lpVtbl->put_Width(webBrowser2, rect.right);
							webBrowser2->lpVtbl->put_Height(webBrowser2, rect.bottom);

							webBrowser2->lpVtbl->Release(webBrowser2);

							// Success
							return 0;
						}
					}

					_iOleClientSiteEx->browserObject->lpVtbl->Close(_iOleClientSiteEx->browserObject, OLECLOSE_NOSAVE);
					_iOleClientSiteEx->browserObject->lpVtbl->Release(_iOleClientSiteEx->browserObject);
				}

				webBrowser2->lpVtbl->Release(webBrowser2);

				GlobalFree(_iOleClientSiteEx);
			}

			// Can't get the web browser's IWebBrowser2!
			return -1;
		}

	case WM_DESTROY:
        {
			IOleObject  *browserObject;
			_IOleClientSiteEx  *_iOleClientSiteEx;

			// Retrieve the browser object's pointer we stored in our window's GWL_USERDATA when we
			// initially attached the browser object to this window.
			if ((_iOleClientSiteEx = (_IOleClientSiteEx *)GetWindowLongPtrW(hwnd, GWLP_USERDATA)))
			{
				// Unembed the browser object, and release its resources.
				browserObject = _iOleClientSiteEx->browserObject;
				browserObject->lpVtbl->Close(browserObject, OLECLOSE_NOSAVE);
				browserObject->lpVtbl->Release(browserObject);
				GlobalFree(_iOleClientSiteEx);
			}

            return TRUE;
        }
    }

    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

WindowHandle osCreateWebView(WindowHandle window)
{
	HWND hWebView;

	hWebView = CreateWindowExW(
			  WS_EX_CLIENTEDGE,
			  L"HWebView",
			  NULL,
			  WS_CHILD | WS_BORDER | WS_TABSTOP | TVS_HASLINES | TVS_HASBUTTONS | TVS_LINESATROOT,
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hWebView, "HWebView");
}

void osWebViewLoadURL(WindowHandle window, PortString url)
{
    IWebBrowser2    *webBrowser2;
    VARIANT         myURL;

    // Get the browser's IWebBrowser2 object.
    if (!GetWebPtrs(window, &webBrowser2, NULL)) {
        VariantInit(&myURL);
        XV2(myURL).vt = VT_BSTR;

        if ((XV3(myURL).bstrVal = SysAllocString(url)) == NULL)
        {
            webBrowser2->lpVtbl->Release(webBrowser2);
            return;
        }

        // Call the Navigate2() function to actually display the page.
        webBrowser2->lpVtbl->Navigate2(webBrowser2, &myURL, 0, 0, 0, 0);

        // Free any resources (including the BSTR we allocated above).
        VariantClear(&myURL);

        // We no longer need the IWebBrowser2 object (ie, we don't plan to call any more functions in it,
        // so we can release our hold on it). Note that we'll still maintain our hold on the browser
        // object.
        webBrowser2->lpVtbl->Release(webBrowser2);
    }
}

void osWebViewLoadHTML(WindowHandle window, PortString html)
{   
    IHTMLDocument2  *htmlDoc2;
    IWebBrowser2    *webBrowser2;
    SAFEARRAY       *sfArray;
    VARIANT         myURL;
    VARIANT         *pVar;

    VariantInit(&myURL);
    XV2(myURL).vt = VT_BSTR;

    // Get the browser's IWebBrowser2 object.
    if (!GetWebPtrs(window, &webBrowser2, NULL))
    {
        // Ok, now the pointer to our IWebBrowser2 object is in 'webBrowser2', and so its VTable is
        // webBrowser2->lpVtbl.

        // Before we can get the IHTMLDocument2, we actually need to have some HTML page loaded in
        // the browser. So, let's load an empty HTML page. Then, once we have that empty page, we
        // can get that IHTMLDocument2 and call its write() to stuff our HTML string into it.

        // Call the IWebBrowser2 object's get_Document so we can get its DISPATCH object. I don't know why you
        // don't get the DISPATCH object via the browser object's QueryInterface(), but you don't.

        // Give a URL that causes the browser to display an empty page.
        XV3(myURL).bstrVal = SysAllocString(&Blank[0]);

        // Call the Navigate2() function to actually display the page.
        webBrowser2->lpVtbl->Navigate2(webBrowser2, &myURL, 0, 0, 0, 0);

        // Wait for blank page to finish loading
        if (WaitOnReadyState(window, READYSTATE_COMPLETE, 1000, webBrowser2) != WORS_DESTROYED)
        {
            SysFreeString(XV3(myURL).bstrVal);

            // Get the browser's IHTMLDocument2 object.
            if (!GetWebPtrs(window, 0, &htmlDoc2))
            {
				static const SAFEARRAYBOUND ArrayBound = {1, 0};

                // Ok, now the pointer to our IHTMLDocument2 object is in 'htmlDoc2', and so its VTable is
                // htmlDoc2->lpVtbl.

                // Our HTML must be in the form of a BSTR. And it must be passed to write() in an
                // array of "VARIENT" structs. So let's create all that.
                if ((sfArray = SafeArrayCreate(VT_VARIANT, 1, (SAFEARRAYBOUND *)&ArrayBound)))
                {
                    if (!SafeArrayAccessData(sfArray, (void **)&pVar))
                    {
                        XV2(*pVar).vt = VT_BSTR;

                        // Store our BSTR pointer in the VARIENT.
                        if ((XV3(*pVar).bstrVal = SysAllocString(html)))
                        {
                            // Pass the VARIENT with its BSTR to write() in order to shove our desired HTML string
                            // into the body of that empty page we created above.
                            htmlDoc2->lpVtbl->write(htmlDoc2, sfArray);

                            // Close the document. If we don't do this, subsequent calls to DisplayHTMLStr
                            // would append to the current contents of the page
                            htmlDoc2->lpVtbl->close(htmlDoc2);

                            // Success. Just set this to something other than VT_BSTR to flag success
                            ++XV2(myURL).vt;

                            // Normally, we'd need to free our BSTR, but SafeArrayDestroy() does it for us
    //                      SysFreeString(pVar->bstrVal);
                        }

                        // Free the array. This also frees the VARIENT that SafeArrayAccessData created for us,
                        // and even frees the BSTR we allocated with SysAllocString
                        SafeArrayDestroy(sfArray);
                    }
                }

                // Release the IHTMLDocument2 object.
                htmlDoc2->lpVtbl->Release(htmlDoc2);
            }
        }
        else
            SysFreeString(XV3(myURL).bstrVal);

        // Release the IWebBrowser2 object.
        webBrowser2->lpVtbl->Release(webBrowser2);
    }
}


static long WINAPI DisplayHTMLStr(HWND, const WCHAR *);

void osGetWebViewReqSize(WindowHandle webview, int *res)
{
	res[0] = 100;
	res[1] = 100;
}
