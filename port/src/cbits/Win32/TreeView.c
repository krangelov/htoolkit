#include "TreeView.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include <commctrl.h>
#include <limits.h>


#define 	TVCOLUMNW				LVCOLUMNW
#define 	TV_NOIMAGE				(-2)
#define 	TV_NOCOLOR				0xFFFFFFFF
#define 	TVIF_TEXTPTR			0x80000000
#define		TVIF_SUBITEM	        0x8000
#define 	TVIF_SUBNUMBER			0x4000
#define 	TVIS_UNTERLINE			0x0001
#define		TVN_COMUMNCLICK			HDN_ITEMCLICK 
#define 	TVN_COMUMNDBLCLICK		HDN_ITEMDBLCLICK
#define		TVI_ROW			        ((HTREEITEM)0xFFFF0004)



#define 	TVC_BK					0					
#define 	TVC_ODD					1					
#define 	TVC_EVEN				2					
#define 	TVC_FRAME				3					
#define 	TVC_TEXT				4					
#define 	TVC_LINE				5					
#define 	TVC_BOX					6					
#define 	TVC_TRACK				7					
#define 	TVC_MARK				8					
#define 	TVC_MARKODD				8					
#define 	TVC_MARKEVEN			9					
#define 	TVC_INSERT				10					



#define 	TVGN_DROPHILITESUB		0x000C				
#define 	TVGN_CARETSUB			0x000D				
#ifndef		TVGN_NEXTSELECTED
#define 	TVGN_NEXTSELECTED		0x000E				
#endif 	
#define 	TVGN_FOCUS				0x000F				
#define 	TVGN_FOCUSSUB			0x0010				
#define 	TVGN_NEXTSELCHILD		0x0011				



#define 	TVCF_FMT				LVCF_FMT
#define 	TVCF_IMAGE				LVCF_IMAGE
#define 	TVCF_TEXT				LVCF_TEXT
#define 	TVCF_WIDTH				LVCF_WIDTH



#define 	TVHT_SUBTOCOL(u)		(((unsigned)u)>>24)	
#define 	TVHT_SUBMASK			0xFF000000			
#define 	TVHT_ONRIGHTSPACE		0x00800000			
#define 	TVHT_ONSUBLABEL			0x00400000			
#define 	TVHT_ONSUBICON			0x00200000			
#define 	TVHT_ONSUBRIGHT			0x00100000			
#define 	TVHT_ONSUBITEM			(TVHT_ONSUBICON|TVHT_ONSUBLABEL)



#define 	TVIR_COLTOSUB(u)		((u)<<24)	
#define 	TVIR_GETCOLUMN			0x00000080
#define 	TVIR_TEXT				0x00000001



#define 	TVIR_EDITCOMBOBOX		0x20000000			
#define 	TVIR_EDITCOMBOLIST		0x40000000			
#define 	TVIR_EDITFULL			0x80000000			



#define 	TVCFMT_CENTER			LVCFMT_CENTER
#define 	TVCFMT_COL_HAS_IMAGES	LVCFMT_COL_HAS_IMAGES
#define 	TVCFMT_IMAGE			LVCFMT_IMAGE
#define 	TVCFMT_LEFT				LVCFMT_LEFT
#define 	TVCFMT_RIGHT			LVCFMT_RIGHT



#define 	TVS_EX_ITEMLINES		0x00010000			
#define 	TVS_EX_ALTERNATECOLOR	0x00020000			
#define 	TVS_EX_SUBSELECT		0x00040000			
#define 	TVS_EX_FULLROWMARK		0x00080000			
#ifndef		TVS_EX_MULTISELECT
#define 	TVS_EX_MULTISELECT		0x00001000			
#endif 	
#ifndef		TVS_EX_AUTOHSCROLL 
#define 	TVS_EX_AUTOHSCROLL 		0x00002000			
#endif 	



#define 	TVM_GETHEADER			(TV_FIRST+80-1)
#define 	TVM_GETCOLUMNCOUNT		(TV_FIRST+80-2)
#define 	TVM_DELETECOLUMN		(TV_FIRST+80-3)
#define 	TVM_INSERTCOLUMN		(TV_FIRST+80-4)
#define 	TVM_SELECTSUBITEM		(TV_FIRST+80-5)
#define 	TVM_SELECTDROP			(TV_FIRST+80-6)
#define 	TVM_SETITEMBKCOLOR		(TV_FIRST+80-7)
#define 	TVM_GETITEMBKCOLOR		(TV_FIRST+80-8)
#define 	TVM_SETITEMTEXTCOLOR	(TV_FIRST+80-9)
#define 	TVM_GETITEMTEXTCOLOR	(TV_FIRST+80-10)
#define 	TVM_GETITEMOFROW		(TV_FIRST+80-11)
#define 	TVM_GETROWOFITEM		(TV_FIRST+80-12)	
#define 	TVM_SETCOLUMN			(TV_FIRST+80-13)
#define 	TVM_GETCOLUMN			(TV_FIRST+80-14)
#define 	TVM_SETCOLUMNWIDTH		(TV_FIRST+80-15)
#define 	TVM_GETCOLUMNWIDTH		(TV_FIRST+80-16)
#define 	TVM_SETUSERDATASIZE		(TV_FIRST+80-17)
#define 	TVM_GETUSERDATASIZE		(TV_FIRST+80-18)
#define 	TVM_GETUSERDATA			(TV_FIRST+80-19)
#ifndef		TVM_GETITEMSTATE
#define 	TVM_GETITEMSTATE		(TV_FIRST+39)
#endif 	
#ifndef		TVM_GETEXTENDEDSTYLE
#define 	TVM_GETEXTENDEDSTYLE	(TV_FIRST+45)
#endif 	
#ifndef		TVM_SETEXTENDEDSTYLE
#define 	TVM_SETEXTENDEDSTYLE	(TV_FIRST+44)
#endif 	
#ifndef		TVM_GETLINECOLOR
#define 	TVM_GETLINECOLOR		(TV_FIRST+41)
#endif 	
#ifndef		TVM_SETLINECOLOR
#define 	TVM_SETLINECOLOR		(TV_FIRST+40)
#endif 	


#ifndef		TVNRET_DEFAULT 
#define		TVNRET_DEFAULT			0
#endif 	
#ifndef		TVNRET_SKIPOLD 
#define		TVNRET_SKIPOLD			1
#endif 	
#ifndef		TVNRET_SKIPNEW 
#define		TVNRET_SKIPNEW			2
#endif 	

#define 	TreeList_DeleteAllItems(h)					((BOOL      )SendMessage(h,TVM_DELETEITEM,0,TVI_ROOT))
#define		TreeList_DeleteItem(h,i)					((BOOL      )SendMessage(h,TVM_DELETEITEM,0,(LPARAM)(HTREEITEM)(i)))
#define		TreeList_Expand(h,i,c)						((BOOL      )SendMessage(h,TVM_EXPAND,(WPARAM)c,(LPARAM)(HTREEITEM)(i)))
#define		TreeList_GetHeader(h)						((HWND      )SendMessage(h,TVM_GETHEADER,0,0))
#define		TreeList_DeleteColumn(h,i)     				((BOOL      )SendMessage(h,TVM_DELETECOLUMN,(WPARAM)(int)(i),0))
#define		TreeList_InsertColumn(h,i,p)				((INT       )SendMessage(h,TVM_INSERTCOLUMN,(WPARAM)(int)(i),(LPARAM)(const TVCOLUMNW*)(p)))
#define 	TreeList_HitTest(h,p)						((HTREEITEM )SendMessage(h,TVM_HITTEST,0,(LPARAM)(LPTVHITTESTINFO)(p)))
#define 	TreeList_GetItemOfRow(h,r)					((HTREEITEM )SendMessage(h,TVM_GETITEMOFROW,0,r))
#define 	TreeList_GetRowOfItem(h,i)					((INT       )SendMessage(h,TVM_GETROWOFITEM,0,(LPARAM)i))
#define		TreeList_GetExtendedStyle(h)				((DWORD     )SendMessage(h,TVM_GETEXTENDEDSTYLE,0,0))
#define		TreeList_SetExtendedStyle(h,d)	  			((DWORD     )SendMessage(h,TVM_SETEXTENDEDSTYLE,0,d))
#define 	TreeList_SetExtendedStyleEx(h,d,m)			((DWORD     )SendMessage(h,TVM_SETEXTENDEDSTYLE,m,d))
#define		TreeList_GetColor(h,i)						((COLORREF  )SendMessage(h,TVM_GETBKCOLOR,(WPARAM)i,0))
#define		TreeList_SetColor(h,i,c) 					((COLORREF  )SendMessage(h,TVM_SETBKCOLOR,(WPARAM)i,c))
#define		TreeList_GetItemBkColor(h,i,s)				((COLORREF  )SendMessage(h,TVM_GETITEMBKCOLOR,(WPARAM)i,s))
#define		TreeList_SetItemBkColor(h,i,s,c) 			((COLORREF  )SendMessage(h,TVM_SETITEMBKCOLOR,((UINT)i)|(s<<24),c))
#define		TreeList_GetItemTextColor(h,i,s)			((COLORREF  )SendMessage(h,TVM_GETITEMTEXTCOLOR,(WPARAM)i,s))
#define		TreeList_SetItemTextColor(h,i,s,c) 			((COLORREF  )SendMessage(h,TVM_SETITEMTEXTCOLOR,((UINT)i)|(s<<24),c))
#define 	TreeList_EnsureVisible(h,i)					((BOOL      )SendMessage(h,TVM_ENSUREVISIBLE,0,(LPARAM)(HTREEITEM)(i)))
#define 	TreeList_EnsureVisibleEx(h,i,s)				((BOOL      )SendMessage(h,TVM_ENSUREVISIBLE,s,(LPARAM)(HTREEITEM)(i)))
#define 	TreeList_SelectDropTargetEx(h,i,s)			((BOOL      )SendMessage(h,TVM_SELECTDROP,(WPARAM)s,(LPARAM)(HTREEITEM)(i)))
#define 	TreeList_SelectSubItem(h,i,s)				((BOOL      )SendMessage(h,TVM_SELECTSUBITEM,(WPARAM)s,(LPARAM)(HTREEITEM)(i)))
#define 	TreeList_Select(h,i,c)						((BOOL      )SendMessage(h,TVM_SELECTITEM,(WPARAM)c,(LPARAM)(HTREEITEM)(i)))
#define		TreeList_EditLabel(h,i,s)				    ((HWND      )SendMessage(h,TVM_EDITLABEL,s,(LPARAM)(HTREEITEM)(i)))
#define		TreeList_EndEditLabelNow(h,c)				((BOOL      )SendMessage(h,TVM_ENDEDITLABELNOW,c,0))
#define		TreeList_GetItem(h,p)						((BOOL      )SendMessage(h,TVM_GETITEM,0,(LPARAM)(TVITEMW*)(p)))
#define		TreeList_GetCount							((BOOL      )SendMessage(h,TVM_GETCOUNT,0,0))
#define		TreeList_GetEditControl(h)					((HWND      )SendMessage(h,TVM_GETEDITCONTROL,s,0))
#define		TreeList_GetImageList(h,i)					((HIMAGELIST)SendMessage(h,TVM_GETIMAGELIST,i,0))
#define		TreeList_GetUserData(h,i)					((LPVOID    )SendMessage(h,TVM_GETUSERDATA,0,(LPARAM)(HTREEITEM)(i)))
#define		TreeList_GetUserDataSize(h)					((INT       )SendMessage(h,TVM_GETUSERDATASIZE,0,0))
#define		TreeList_SetUserDataSize(h,s)				((INT       )SendMessage(h,TVM_SETUSERDATASIZE,0,s))
#define		TreeList_GetIndent							((UINT      )SendMessage(h,TVM_GETINDENT,0,0))
#define		TreeList_GetVisibleCount					((UINT      )SendMessage(h,TVM_GETVISIBLECOUNT,0,0))
#define		TreeList_InsertItem(h,p)					((HTREEITEM )SendMessage(h,TVM_INSERTITEM,0,(LPARAM)(LPTVINSERTSTRUCTW)(p)))
#define		TreeList_CreateDragImage(h,i)				((HIMAGELIST)SendMessage(h,TVM_CREATEDRAGIMAGE, 0, (LPARAM)(HTREEITEM)(i)))
#define		TreeList_CreateDragImageEx(h,i,s)			((HIMAGELIST)SendMessage(h,TVM_CREATEDRAGIMAGE, s, (LPARAM)(HTREEITEM)(i)))
#define		TreeList_SetImageList(h,i,l)				((HIMAGELIST)SendMessage(h,TVM_SETIMAGELIST,i,(LPARAM)(UINT)(HIMAGELIST)(l)))
#define		TreeList_SetIndent(h,i)					    ((BOOL      )SendMessage(h,TVM_SETINDENT,(WPARAM)i,0))
#define		TreeList_SetItem(h,p)					    ((BOOL      )SendMessage(h,TVM_SETITEM,0,(LPARAM)(const TVITEMW*)(p)))
#define		TreeList_SortChildren(h,i,r)				((BOOL      )SendMessage(h,TVM_SORTCHILDREN  ,(WPARAM)r,(LPARAM)(HTREEITEM)(i)))
#define		TreeList_SortChildrenCB(h,p,r)				((BOOL      )SendMessage(h,TVM_SORTCHILDRENCB,(WPARAM)r,(LPARAM)(LPTV_SORTCB)(p)))
#define		TreeList_SetColumn(h,i,p)					((BOOL      )SendMessage(h,TVM_SETCOLUMN,i,(LPARAM)(const TVCOLUMNW*)(p)))
#define		TreeList_GetColumn(h,i,p)					((BOOL      )SendMessage(h,TVM_GETCOLUMN,i,(LPARAM)(TVCOLUMNW*)(p)))
#define		TreeList_GetColumnCount(h)					((INT       )SendMessage(h,TVM_GETCOLUMNCOUNT,0,0))
#define		TreeList_SetColumnWidth(h,i,w)				((BOOL      )SendMessage(h,TVM_SETCOLUMNWIDTH,i,w))
#define		TreeList_GetColumnWidth(h,i)				((INT       )SendMessage(h,TVM_GETCOLUMNWIDTH,i,0))

#define		TreeList_GetItemRect(h,i,s,p,c)			    (*(HTREEITEM*)p =(i),(BOOL)::SendMessage(h,TVM_GETITEMRECT,(WPARAM)((c)|((s)<<8)),(LPARAM)(RECT*)(p)))

#define		TreeList_SelectItem(h,i)					TreeList_Select(h,i,TVGN_CARET)
#define 	TreeList_SelectDropTarget(h,i)				TreeList_Select(h,i,TVGN_DROPHILITE)
#define 	TreeList_SelectSetFirstVisible(h,i)			TreeList_Select(h,i,TVGN_FIRSTVISIBLE)

#define		TreeList_GetChild(h,i)						TreeView_GetNextItem(h, i,     TVGN_CHILD)
#define 	TreeList_GetParent(h, i)         			TreeView_GetNextItem(h, i,     TVGN_PARENT)
#define 	TreeList_GetNextSibling(h,i)    			TreeView_GetNextItem(h, i,     TVGN_NEXT)
#define 	TreeList_GetPrevSibling(h,i)    			TreeView_GetNextItem(h, i,     TVGN_PREVIOUS)
#define 	TreeList_GetNextSelected(h,i)			    TreeView_GetNextItem(h, i,     TVGN_NEXTSELECTED)
#define		TreeList_GetNextSelectedChild(h,i)			TreeView_GetNextItem(h, i,	   TVGN_NEXTSELCHILD)
#define 	TreeList_GetNextVisible(h,i)    			TreeView_GetNextItem(h, i,     TVGN_NEXTVISIBLE)
#define 	TreeList_GetPrevVisible(h,i)    			TreeView_GetNextItem(h, i,     TVGN_PREVIOUSVISIBLE)
#define 	TreeList_GetSelection(h)					TreeView_GetNextItem(h, NULL,  TVGN_CARET)
#define 	TreeList_GetDropHilight(h)					TreeView_GetNextItem(h, NULL,  TVGN_DROPHILITE)
#define 	TreeList_GetFirstVisible(h)				    TreeView_GetNextItem(h, NULL,  TVGN_FIRSTVISIBLE)
#define		TreeList_GetLastVisible(h)					TreeView_GetNextItem(h, NULL,  TVGN_LASTVISIBLE)
#define 	TreeList_GetRoot(h)							TreeView_GetNextItem(h, NULL,  TVGN_ROOT)
#define		TreeList_GetFocus(h)						TreeView_GetNextItem(h, NULL,  TVGN_FOCUS)
#define		TreeList_GetFocusColumn(h)					((int)TreeView_GetNextItem(h, NULL,  TVGN_FOCUSSUB))
#define		TreeList_GetSelectionColumn(h)				((int)TreeView_GetNextItem(h, NULL,  TVGN_CARETSUB))
#define		TreeList_GetDropHilightColumn(h)			((int)TreeView_GetNextItem(h, NULL,  TVGN_DROPHILITESUB))

#ifndef		WM_MOUSEWHEEL
#define		WM_MOUSEWHEEL		0x020A
#endif
#ifndef		WHEEL_DELTA
#define		WHEEL_DELTA			120
#endif


#define 	MAX_COLUMNS			32
#define		MAX_COLORS			11
#define 	EN_RETURN			0x1578
#define 	EN_ESCAPE			0x1579
#define 	ID_TOOLTIPCHECK		0x3912
#define 	SORT_NOUPDATE		1234567
#define 	FIRST_LINE			0xFFFFFFFE
#define 	I_CCB				I_CHILDRENCALLBACK
#define 	U(h)				((size_t)(h))
#define 	GetHandle(h)		((TreeListData*)GetWindowLongPtrW(h,0))
#define 	TVIF_ALL			(TVIF_CHILDREN|TVIF_HANDLE|TVIF_IMAGE|TVIF_PARAM|TVIF_SELECTEDIMAGE|TVIF_STATE|TVIF_TEXT)
#define 	UNLOCK(d)			ReleaseSemaphore(d->hSem,1,NULL)
#define 	LOCK(d)				WaitForSingleObject(d->hSem,INFINITE)
#define 	TVIS_BASEFLAGS		(TVIS_EXPANDED|TVIS_EXPANDEDONCE|TVIS_EXPANDPARTIAL|TVIS_SELECTED)
#define 	TVIS_TRACKED		(TVIX_TRACKED<<16)
#define 	TVIS_BKCOLOR		(TVIX_BKCOLOR<<16)

#define 	TVIS_TEXTCOLOR		(TVIX_TEXTCOLOR<<16)
#define 	TVC_DESELECT		0x8000
#define 	DEFAULT_IDENT		19
#define 	DEFAULT_SHIFT		7


#define 	TVIX_VARBUTTON		0x01						
#define 	TVIX_HASBUTTON		0x02						
#define 	TVIX_HASIMAGE		0x04						
#define 	TVIX_TRACKED		0x08						
#define 	TVIX_TEXTCOLOR		0x10						
#define 	TVIX_BKCOLOR		0x20						
#define 	TVIX_FOCUSED		0x40						


typedef struct {
	LPARAM		lParam;										
	LPWSTR		pText;										
	UINT		uState;										
	int			iImage;										
	int			iSelectedImage; 							
	unsigned	uShowPos;									
	size_t		uFirstChild;
	size_t		uLastChild;
	size_t		uPrevItem;
	size_t		uNextItem; 
	size_t		uParent;
	unsigned 	uLevel;										
	int 		iTextPixels;								
	WORD		uTextSize;									
	BYTE		bCallback;									
	BYTE		bFlags;										
	COLORREF	uColorText;									
	COLORREF	uColorBk;									
} BaseItem;

typedef struct {
	LPWSTR		pText;										
	UINT		uState;										
	int			iImage;										
	int 		iTextPixels;								
	WORD		uTextSize;									
	BYTE		bCallback;									
	BYTE		bFlags;										
	COLORREF	uColorText;									
	COLORREF	uColorBk;									
} ExtraItem;

typedef struct {
	HWND		hWnd;										
	HANDLE		hSem;										
	HIMAGELIST	hStates;									
	HIMAGELIST	hImages;									
	HFONT		hFontN;										
	HFONT		hFontB;										
	HFONT		hFontL;										
	HWND		hEdit;										
	HWND		hHeader;									
	HWND		hToolTip;									
	WNDPROC		pToolTipProc;								
	COLORREF	uColors[MAX_COLORS];						
	int			iFontHeight;								
	int			iFontLine;									
	int			iFontOff;									
	int			iStateCheck;								
	int			iStatesXsize;								
	int			iStatesYsize;								
	int			iImagesXsize;								
	int			iImagesYsize;								
	int			iRowHeight;									
	int			iIdent;										
	int			iShift;										
	int			iMaxSizeX;									
	unsigned	uItemPosCount;								
	unsigned   *pItemPos;									
	BaseItem  **pTreeItems;									
	ExtraItem **pExtraItems[MAX_COLUMNS-1];					
	unsigned	uTreeItemsMax;								
	unsigned	uTreeItemsCount;							
	unsigned	uNextSeachPos;								
	unsigned	uUserDataSize;								
	size_t		uFirstChild;								
	size_t		uLastChild;									
	unsigned	uScrollY;									
	unsigned	uScrollX;									
	unsigned	uSizeX;										
	unsigned	uSizeY;										
	unsigned	uSizeYsub;									
	unsigned	uStyle;										
	unsigned	uStyleEx;									
	unsigned	uStartPixel;								
	unsigned	uMaxEnties;									
	unsigned	uPageEnties;								
	unsigned	uColumnCount;								
	unsigned	uColumnCountVar;							
	unsigned	uSelectedCount;								
	unsigned	uSelectedBase;								
	size_t		uSelectedItem;								
	unsigned	uSelectedSub;								
	unsigned	uFocusItem;									
	unsigned	uFocusSub;									
	unsigned	uToolTipItem;								
	unsigned	uToolTipSub;								
	unsigned	uEditMode;									
	unsigned	uEditItem;									
	unsigned	uEditSub;									
	unsigned	uOldXPage;									
	unsigned	uOldXCount;									
	unsigned	uOldYPage;									
	unsigned	uOldYCount;									
	unsigned	uTrackedItem;								
	unsigned	uTrackedSub;								
	size_t		uInsertMark;								
	unsigned	uDragFlags;									
	size_t		uDragItem;									
	unsigned	uDragSub;									
	WCHAR		cTempText1[260];							
	WCHAR		cTempText2[260];							
	int			iColumnXpos  [MAX_COLUMNS+2];				
	char		cColumnVar   [MAX_COLUMNS];					
	char		cColumnAlign [MAX_COLUMNS];					
	char		cColorChanged[MAX_COLORS];					
	char 		cFixedHeight;								
	char		cHasFocus;									
	char		cIsEnabled;									
	char		cLockChanges;								
	char		uEditCb;									
} TreeListData;

static LONG				lWindowCount  = -1;
static HPEN				hPatternPen   = NULL;
static HFONT			hDefaultFontN = NULL;
static HFONT			hDefaultFontB = NULL;
static RECT				sToolRect={-2,0,2,64};
static void				TreeListDraw(HWND hWnd,HDC hDc,RECT *pRect);
static int				TreeListSelectItem  (TreeListData *pData,size_t uItem,size_t uSubItem,int iMode);
static int				TreeListGetItemRect (TreeListData *pData,size_t uItem,unsigned uFlags,RECT *pRect);
static int				TreeListEndLabelEdit(TreeListData *pData,int iCancel);


static void GlobalInit()
{
	LOGBRUSH	sLog;
	long		lCount;

	lCount = InterlockedIncrement(&lWindowCount);
	if (lCount>0) return;

	sLog.lbColor = GetSysColor(COLOR_BTNSHADOW);
	sLog.lbStyle = PS_SOLID;
	sLog.lbHatch = 0;
	hPatternPen  = ExtCreatePen(PS_COSMETIC|PS_ALTERNATE,1,&sLog,0,NULL);

    if (!hPatternPen) {
		hPatternPen = CreatePen(PS_DOT,1,RGB(0,0,0));
	}
}


static void GlobalDeinit()
{
	int	lCount;

	lCount=InterlockedDecrement(&lWindowCount);
	if (lCount>=0) return;

    if (hDefaultFontN) {
        DeleteObject(hDefaultFontN);
        hDefaultFontN = NULL;
    }

    if (hDefaultFontB) {
        DeleteObject(hDefaultFontB);
        hDefaultFontB = NULL;
    }

    if (hPatternPen) {
        DeleteObject(hPatternPen);
        hPatternPen = NULL;
    }
}

static LRESULT SendNotify(TreeListData *pData, NMHDR *pNotify)
{
	pNotify->hwndFrom	= pData->hWnd;
    pNotify->idFrom		= GetWindowLongPtrW(pNotify->hwndFrom,GWLP_ID);

	return SendMessage(GetParent(pNotify->hwndFrom),WM_NOTIFY,pNotify->idFrom,(LPARAM)pNotify);
}

static void CallbackEntry(TreeListData *pData,BaseItem *pEntry,size_t uItem,unsigned uFlags,int *iImage,unsigned *uTextSize,LPCWSTR *pText)
{
	NMTVDISPINFOW sInfo;

	sInfo.item.mask				= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|uFlags;
	sInfo.item.lParam			= pEntry->lParam;
	sInfo.item.hItem			= (HTREEITEM) uItem;
	sInfo.item.state			= pEntry->uState;
	sInfo.item.stateMask		= 0xFFFFFFFF;
	sInfo.item.iImage			= I_IMAGECALLBACK;
	sInfo.item.iSelectedImage	= I_IMAGECALLBACK;
	sInfo.item.cChildren		= I_CHILDRENCALLBACK;

	if (uFlags & TVIF_TEXT) {
		if (*uTextSize) {
			pData->cTempText2[sizeof(pData->cTempText2)/sizeof(WCHAR)-1]	= 0;
			pData->cTempText2[0]	= 0;
			sInfo.item.pszText		= pData->cTempText2;
			sInfo.item.cchTextMax	= sizeof(pData->cTempText2)/sizeof(WCHAR)-1;
		} else {
			pData->cTempText1[sizeof(pData->cTempText1)/sizeof(WCHAR)-1]	= 0;
			pData->cTempText1[0]	= 0;
			sInfo.item.pszText		= pData->cTempText1;
			sInfo.item.cchTextMax	= sizeof(pData->cTempText1)/sizeof(WCHAR)-1;
		}
	} else {
		sInfo.item.pszText		= 0;
		sInfo.item.cchTextMax	= 0;
	}

	sInfo.hdr.hwndFrom	= pData->hWnd;
    sInfo.hdr.idFrom	= GetWindowLongPtrW(pData->hWnd,GWLP_ID);
	sInfo.hdr.code		= TVN_GETDISPINFO;

	UNLOCK(pData);
	SendMessage(GetParent(sInfo.hdr.hwndFrom),WM_NOTIFY,sInfo.hdr.idFrom,(LPARAM)&sInfo);
	LOCK(pData);

	if (uFlags & TVIF_IMAGE) {
		if (!(pEntry->uState&TVIS_SELECTED))
			*iImage = sInfo.item.iImage;
	} else if (uFlags & TVIF_SELECTEDIMAGE) {
		if (pEntry->uState&TVIS_SELECTED)
			*iImage = sInfo.item.iSelectedImage;
	}

	if (uFlags&TVIF_CHILDREN) {
		switch(sInfo.item.cChildren) {
		case  0:
			pEntry->bFlags    &= ~TVIX_HASBUTTON;
			pEntry->bFlags    &=  TVIX_VARBUTTON;
			break;
		case  1:
			pEntry->bFlags    &=  TVIX_VARBUTTON;
			pEntry->bFlags    |=  TVIX_HASBUTTON;
			break;
		default:
			pEntry->bFlags    |=  TVIX_VARBUTTON;

			if (pEntry->uFirstChild)
				pEntry->bFlags |=  TVIX_HASBUTTON;
			else
				pEntry->bFlags &= ~TVIX_HASBUTTON;
		}
	}

	if (uFlags & TVIF_TEXT) {
		*pText		= sInfo.item.pszText;
		*uTextSize	= pslen(sInfo.item.pszText);
		pEntry->iTextPixels = 0;
 	}
}

static void CallbackExtra(TreeListData *pData,BaseItem *pEntry,ExtraItem *pExtra,size_t uItem,unsigned uSub,unsigned uFlags,int *iImage,unsigned *uTextSize,LPCWSTR *pText)
{
	NMTVDISPINFOW sInfo;

	sInfo.item.mask				= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBITEM|uFlags;
	sInfo.item.lParam			= pEntry->lParam;
	sInfo.item.hItem			= (HTREEITEM)uItem;
	sInfo.item.state			= pExtra->uState;
	sInfo.item.state		   |=(pEntry->uState&TVIS_BASEFLAGS);
	sInfo.item.stateMask		= 0xFFFFFFFF;
	sInfo.item.iImage			= I_IMAGECALLBACK;
	sInfo.item.iSelectedImage	= I_IMAGECALLBACK;
	sInfo.item.cChildren		= uSub;

	if (uFlags & TVIF_TEXT) {
		pData->cTempText1[sizeof(pData->cTempText1)/sizeof(WCHAR)-1]	= 0;
		pData->cTempText1[0]	= 0;
		sInfo.item.pszText		= pData->cTempText1;
		sInfo.item.cchTextMax	= sizeof(pData->cTempText1)/sizeof(WCHAR)-1;
	} else {
		sInfo.item.pszText		= 0;
		sInfo.item.cchTextMax	= 0;
	}

	sInfo.hdr.hwndFrom	= pData->hWnd;
    sInfo.hdr.idFrom	= GetWindowLongPtrW(pData->hWnd,GWLP_ID);
	sInfo.hdr.code		= TVN_GETDISPINFO;

	UNLOCK(pData);
	SendMessage(GetParent(sInfo.hdr.hwndFrom),WM_NOTIFY,sInfo.hdr.idFrom,(LPARAM)&sInfo);
	LOCK(pData);

	if (uFlags&TVIF_IMAGE)
		*iImage = sInfo.item.iImage;
	if (uFlags&TVIF_TEXT) {
		*pText		= sInfo.item.pszText;
		*uTextSize	= pslen((PortString) sInfo.item.pszText);
	}
}

static LRESULT CALLBACK EditProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	WNDPROC	pProc = (WNDPROC) GetWindowLongPtrW(hWnd,GWLP_USERDATA);

	if (uMsg==WM_KEYDOWN) {
		if (wParam==VK_RETURN) {
			SendMessage(GetParent(hWnd),WM_COMMAND,MAKELONG(3,EN_RETURN),(LPARAM)hWnd);
			return 0;
		}

		if(wParam==VK_ESCAPE) {
			SendMessage(GetParent(hWnd),WM_COMMAND,MAKELONG(3,EN_ESCAPE),(LPARAM)hWnd);
			return 0;
		}
	} else if (uMsg==WM_COMMAND) {
		if (wParam==MAKELONG(3,EN_ESCAPE) || wParam==MAKELONG(3,EN_RETURN)) {
			SendMessage(GetParent(hWnd),WM_COMMAND,wParam,(LPARAM)hWnd);
			return 0;
		}
	} else {
		if (uMsg==WM_GETDLGCODE) {
			return DLGC_WANTALLKEYS;
		}
	}

	return CallWindowProc(pProc,hWnd,uMsg,wParam,lParam);
}

static LRESULT CALLBACK ToolTipProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	TreeListData   *pData;
	WCHAR			cText[260];
	PAINTSTRUCT		sPaint;
	WNDPROC			pProc;
	RECT			sRect;
	HDC				hDc;
	int				iLen;

	pData = (TreeListData*)GetWindowLongPtrW(hWnd,GWLP_USERDATA);
	pProc = pData->pToolTipProc;

	if(uMsg==WM_PAINT) {
		GetClientRect(hWnd,&sRect);
		hDc =BeginPaint   (hWnd,&sPaint);
		iLen=CallWindowProc(pProc,hWnd,WM_GETTEXT,sizeof(cText)/sizeof(WCHAR),(LPARAM)cText);
		SelectObject(hDc,(HFONT)CallWindowProc(pProc,hWnd,WM_GETFONT,0,0));
		SetTextAlign(hDc,TA_LEFT|TA_TOP);
		SetTextColor(hDc,GetSysColor(COLOR_INFOTEXT));
		SetBkColor  (hDc,GetSysColor(COLOR_INFOBK));
		ExtTextOutW (hDc,2+pData->iFontOff,0,ETO_OPAQUE,&sRect,cText,iLen,NULL);

		EndPaint(hWnd,&sPaint);
		return 0;
	}

	if (uMsg==WM_SETFOCUS) {
		SetFocus((HWND)wParam);
		return 0;
	}

	if (uMsg==WM_ACTIVATE) {
		if (LOWORD(wParam) != WA_INACTIVE) {
			SetActiveWindow((HWND)lParam);
		}

		return 0;
	}

	return CallWindowProc(pProc,hWnd,uMsg,wParam,lParam);
}

static void CreateToolTip(TreeListData *pData)
{
	if (pData->hToolTip) return;

	pData->hToolTip	    = CreateWindowW(L"STATIC",NULL,WS_POPUP|WS_BORDER|WS_CLIPSIBLINGS,0,0,0,0,pData->hWnd,NULL,ghModule,NULL);
	pData->pToolTipProc = (WNDPROC)GetWindowLongPtrW(pData->hToolTip,GWLP_WNDPROC);
	SetWindowLongPtrW(pData->hToolTip,GWLP_USERDATA,(LPARAM)pData);
	SetWindowLongPtrW(pData->hToolTip,GWLP_WNDPROC ,(LPARAM)ToolTipProc);
	SetWindowLongPtrW(pData->hToolTip,GWLP_ID,2);
}

static void CreateStateImageList(TreeListData *pData)
{
	BITMAPINFO	sInfo;
	BYTE		aMem[0x1000];
	HDC			hDcSrc;
	HDC			hDc;
	HBITMAP		hBmp;
	HBITMAP		hBmpNew;
	RECT		sRect;
	int			iBits;

	if (pData->hStates)
		return;

	hDcSrc	= GetDC(NULL);
	hDc		= CreateCompatibleDC(NULL);
	hBmp    = CreateCompatibleBitmap(hDcSrc,16*3,16);

	SelectObject(hDc,hBmp);
	SelectObject(hDc,GetStockObject(NULL_PEN));
	SetBkMode   (hDc,OPAQUE);
	SetBkColor	(hDc,GetSysColor(COLOR_WINDOW));
	SelectObject(hDc,GetSysColorBrush(COLOR_HIGHLIGHT));
	Rectangle   (hDc,-1,-1,16*3+2,16+2);

	sRect.top	  = 8-6;
	sRect.bottom  = 8+7;
	sRect.left    = 16*1+8-7;
	sRect.right   = 16*1+8+6;

	DrawFrameControl(hDc,&sRect,DFC_BUTTON,DFCS_BUTTONCHECK|DFCS_FLAT);

	sRect.left    = 16*2+8-7;
	sRect.right   = 16*2+8+6;

	DrawFrameControl(hDc,&sRect,DFC_BUTTON,DFCS_BUTTONCHECK|DFCS_CHECKED|DFCS_FLAT);

	iBits = GetDeviceCaps(hDc,BITSPIXEL);

	sInfo.bmiHeader.biSize		  	= sizeof(BITMAPINFOHEADER);
	sInfo.bmiHeader.biWidth			= 16*3;
	sInfo.bmiHeader.biHeight		= 16;
	sInfo.bmiHeader.biPlanes	  	= 1;
	sInfo.bmiHeader.biBitCount	  	= (WORD)iBits;
	sInfo.bmiHeader.biCompression 	= BI_RGB;
	sInfo.bmiHeader.biSizeImage		= 0;
	sInfo.bmiHeader.biXPelsPerMeter	= 0;
	sInfo.bmiHeader.biYPelsPerMeter	= 0;
	sInfo.bmiHeader.biClrUsed		= (iBits>8)? 0:1<<iBits;;
	sInfo.bmiHeader.biClrImportant	= (iBits>8)? 0:1<<iBits;;

	GetDIBits(hDc,hBmp,0,0 ,NULL,&sInfo,(iBits>8)?DIB_RGB_COLORS:DIB_PAL_COLORS);
	GetDIBits(hDc,hBmp,0,16,aMem,&sInfo,(iBits>8)?DIB_RGB_COLORS:DIB_PAL_COLORS);

	hBmpNew = CreateCompatibleBitmap(hDc,16*3,16);

	SetDIBits(hDc,hBmpNew,0,16,aMem,&sInfo,(iBits>8)?DIB_RGB_COLORS:DIB_PAL_COLORS);

	pData->hStates		= ImageList_Create(16,16,ILC_COLORDDB|ILC_MASK,3,14);
	pData->iStatesXsize	= 16;
	pData->iStatesYsize	= 16;
	pData->iStateCheck	= 1;

	ImageList_AddMasked(pData->hStates,hBmpNew,GetSysColor(COLOR_HIGHLIGHT));

	DeleteObject(hBmpNew);
	DeleteObject(hBmp);
	DeleteDC(hDc);
	ReleaseDC(NULL,hDcSrc);
}

static HIMAGELIST CreateDragImage(TreeListData *pData,size_t uItem,unsigned uSub)
{
	ExtraItem  *pExtra;
	BaseItem   *pEntry;
	HIMAGELIST	hList;
	BITMAPINFO	sInfo;
	BYTE	   *pMem;
	HDC			hDcSrc;
	HDC			hDc;
	HBITMAP		hBmp;
	HBITMAP		hBmpNew;
	RECT		sRect;
	unsigned	uTSize;
	int			iAdd;
	int			iBits;
	int			iWidth;
	int			iHeigh;
	int			iImage;
	int			iYpos;
	LPCWSTR		pText;

	if (uItem>pData->uTreeItemsMax)return NULL;

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry)return 0;

	iHeigh = pData->iFontHeight;

	if (uSub)												
	{
		if(uSub>=pData->uColumnCount)return 0;

		pExtra = pData->pExtraItems[uSub-1][uItem];
		if(!pExtra)
		{
			pText  = L"????";
			uTSize = 4;
			iImage = -1;
			iWidth = pData->iFontHeight*4;
		} else {
			pText  = pExtra->pText;
			uTSize = pExtra->uTextSize;
			iImage = pExtra->iImage;
			iWidth = pExtra->iTextPixels;

			if (pExtra->bCallback & (TVIF_IMAGE|TVIF_TEXT))
			{
				CallbackExtra(pData,pEntry,pExtra,uItem,uSub,pExtra->bCallback,&iImage,&uTSize,&pText);
			}
		}
	} else {													
		pText  = pEntry->pText;
		uTSize = pEntry->uTextSize;
		iImage = pEntry->iImage;
		iWidth = pEntry->iTextPixels;

		if(pEntry->bCallback&(TVIF_IMAGE|TVIF_TEXT))
		{
			CallbackEntry(pData,pEntry,uItem,pEntry->bCallback,&iImage,&uTSize,&pText);
		}
	}

	if (pData->hImages && iImage>=0)	{
		if(iHeigh<pData->iImagesYsize)iHeigh=pData->iImagesYsize;
		iAdd    = pData->iImagesXsize+2;
		iWidth += iAdd;
	} else {
		iAdd   = 0;
		iImage = 1;
	}

	if (iWidth>240) iWidth=240;
	if (iHeigh>32 ) iHeigh=32;

	pMem = malloc(iHeigh*(iWidth+4)*4+1024);
	if (!pMem)
		return NULL;

	hDcSrc	= GetDC(NULL);
	hDc		= CreateCompatibleDC(NULL);
	hBmp    = CreateCompatibleBitmap(hDcSrc,iWidth,iHeigh);

	SelectObject(hDc,hBmp);
	SelectObject(hDc,GetStockObject(NULL_PEN));
	SelectObject(hDc,(pEntry->uState&TVIS_BOLD)? pData->hFontB:pData->hFontN);
	SetTextColor(hDc,pData->uColors[TVC_TEXT]);
	SetBkColor	(hDc,RGB(123,77,91));

	sRect.top	  = 0;
	sRect.bottom  = iHeigh;
	sRect.left    = 0;
	sRect.right   = iWidth;
	iYpos		  =(iHeigh-pData->iFontHeight)/2;

	ExtTextOutW(hDc,iAdd,iYpos,ETO_OPAQUE|ETO_CLIPPED,&sRect,pText,uTSize,NULL);

	if(iImage>=0)
	{
		SetBkColor(hDc,GetSysColor(COLOR_WINDOW));
		ImageList_Draw(pData->hImages,iImage,hDc,0,0,ILD_TRANSPARENT);
	}

	iBits = GetDeviceCaps(hDc,BITSPIXEL);

	sInfo.bmiHeader.biSize		  	= sizeof(BITMAPINFOHEADER);
	sInfo.bmiHeader.biWidth			= iWidth;
	sInfo.bmiHeader.biHeight		= iHeigh;
	sInfo.bmiHeader.biPlanes	  	= 1;
	sInfo.bmiHeader.biBitCount	  	= (WORD)iBits;
	sInfo.bmiHeader.biCompression 	= BI_RGB;
	sInfo.bmiHeader.biSizeImage		= 0;
	sInfo.bmiHeader.biXPelsPerMeter	= 0;
	sInfo.bmiHeader.biYPelsPerMeter	= 0;
	sInfo.bmiHeader.biClrUsed		= (iBits>8)? 0:1<<iBits;;
	sInfo.bmiHeader.biClrImportant	= (iBits>8)? 0:1<<iBits;;

	GetDIBits(hDc,hBmp,0,0     ,NULL,&sInfo,(iBits>8)?DIB_RGB_COLORS:DIB_PAL_COLORS);
	GetDIBits(hDc,hBmp,0,iHeigh,pMem,&sInfo,(iBits>8)?DIB_RGB_COLORS:DIB_PAL_COLORS);

	hBmpNew = CreateCompatibleBitmap(hDc,iWidth,iHeigh);

	SetDIBits(hDc,hBmpNew,0,iHeigh,pMem,&sInfo,(iBits>8)?DIB_RGB_COLORS:DIB_PAL_COLORS);

	hList = ImageList_Create(iWidth,iHeigh,ILC_COLORDDB|ILC_MASK,1,0);

	ImageList_AddMasked(hList,hBmpNew,RGB(123,77,91));

	DeleteObject(hBmpNew);
	DeleteObject(hBmp);
	DeleteDC(hDc);
	ReleaseDC(NULL,hDcSrc);

	free(pMem);

	return hList;
}

static void UpdateTreeColors(TreeListData *pData)
{
	if (!pData->cColorChanged[TVC_BK      ]) pData->uColors[TVC_BK      ]  = GetSysColor(COLOR_WINDOW);
	if (!pData->cColorChanged[TVC_BOX     ]) pData->uColors[TVC_BOX     ]  = GetSysColor(COLOR_BTNSHADOW);
	if (!pData->cColorChanged[TVC_EVEN    ]) pData->uColors[TVC_EVEN    ]  = GetSysColor(COLOR_WINDOW);
	if (!pData->cColorChanged[TVC_TEXT    ]) pData->uColors[TVC_TEXT    ]  = GetSysColor(COLOR_WINDOWTEXT);
	if (!pData->cColorChanged[TVC_LINE    ]) pData->uColors[TVC_LINE    ]  = GetSysColor(COLOR_WINDOWTEXT);
	if (!pData->cColorChanged[TVC_FRAME   ]) pData->uColors[TVC_FRAME   ]  = GetSysColor(COLOR_3DFACE);
	if (!pData->cColorChanged[TVC_TRACK   ]) pData->uColors[TVC_TRACK   ]  = GetSysColor(COLOR_WINDOWTEXT)^RGB(0,0,255);
	if (!pData->cColorChanged[TVC_INSERT  ]) pData->uColors[TVC_INSERT  ]  = GetSysColor(COLOR_INFOBK);
	if (!pData->cColorChanged[TVC_ODD     ]) pData->uColors[TVC_ODD     ]  = GetSysColor(COLOR_INFOBK);
	if (!pData->cColorChanged[TVC_MARKODD ]) pData->uColors[TVC_MARKODD ]  =((pData->uColors[TVC_ODD     ]>>3)&0x1F1F1F)*7;
	if (!pData->cColorChanged[TVC_MARKODD ]) pData->uColors[TVC_MARKODD ] +=((GetSysColor(COLOR_HIGHLIGHT)>>3)&0x1F1F1F)*1;
	if (!pData->cColorChanged[TVC_MARKEVEN]) pData->uColors[TVC_MARKEVEN]  =((pData->uColors[TVC_EVEN    ]>>3)&0x1F1F1F)*7;
	if (!pData->cColorChanged[TVC_MARKEVEN]) pData->uColors[TVC_MARKEVEN] +=((GetSysColor(COLOR_HIGHLIGHT)>>3)&0x1F1F1F)*1;
}

static int UpdateHeight(TreeListData *pData)
{
	int		iHeight;
	RECT	sRect;

	if (pData->cFixedHeight)return 0;

	iHeight = 10;

	if (pData->hStates)
	if (iHeight<pData->iStatesYsize+2) iHeight=pData->iStatesYsize+2;
	if (iHeight<pData->iImagesXsize+2) iHeight=pData->iImagesXsize+2;
	if (iHeight<pData->iFontHeight +2) iHeight=pData->iFontHeight +2;
	if (pData->uStyleEx&TVS_EX_ITEMLINES) iHeight++;
	if (pData->uStyle  &TVS_NONEVENHEIGHT && (iHeight&1)) iHeight++;
	if (pData->iRowHeight==iHeight) return 0;

	pData->iRowHeight	 = iHeight;
	pData->uMaxEnties	 = pData->uSizeY;
	pData->uMaxEnties	-= pData->uStartPixel;
	pData->uPageEnties	 = pData->uMaxEnties;
	pData->uMaxEnties	+= pData->iRowHeight-1;
	pData->uMaxEnties   /= pData->iRowHeight;
	pData->uPageEnties  /= pData->iRowHeight;

	GetClientRect (pData->hWnd,&sRect);
	InvalidateRect(pData->hWnd,&sRect,FALSE);

	return 1;
}

static int UpdateRect(TreeListData *pData,size_t uItem,unsigned uSub)
{
	BaseItem   *pEntry;
	RECT		sRect;
	unsigned	uPos;

    pEntry = pData->pTreeItems[uItem];
	if(!pEntry || !pEntry->uShowPos)return 0;				

	uPos = pEntry->uShowPos-pData->uScrollY-1;
	if(uPos>=pData->uMaxEnties)return 0;					

	sRect.left    = pData->iColumnXpos[uSub  ];
	sRect.left   -= pData->uScrollX;
	sRect.right	  = pData->iColumnXpos[uSub+1];
	sRect.right  -= pData->uScrollX;
	sRect.top     = pData->uStartPixel;
	sRect.top    += pData->iRowHeight*uPos;
	sRect.bottom  = pData->iRowHeight+sRect.top;

	InvalidateRect(pData->hWnd,&sRect,FALSE);

	return 1;
}

static int UpdateRow(TreeListData *pData,size_t uItem)
{
	BaseItem   *pEntry;
	RECT		sRect;
	unsigned	uPos;

    pEntry = pData->pTreeItems[uItem];
	if (!pEntry || !pEntry->uShowPos)
		return 0;

	uPos = pEntry->uShowPos-pData->uScrollY-1;
	if (uPos>=pData->uMaxEnties)
		return 0;

	sRect.left    = 0;
	sRect.right   = pData->uSizeX;
	sRect.top     = pData->uStartPixel;
	sRect.top    += pData->iRowHeight*uPos;
	sRect.bottom  = pData->iRowHeight+sRect.top;
	InvalidateRect(pData->hWnd,&sRect,FALSE);

	return 1;
}

static void UpdateView(TreeListData *pData)
{
	RECT sRect;

	GetClientRect (pData->hWnd,&sRect);
	sRect.top =    pData->uStartPixel;
	InvalidateRect(pData->hWnd,&sRect,FALSE);
}

static void UpdateScrollX(TreeListData *pData)
{
	SCROLLINFO  sInfo;
	unsigned	uSize;
	unsigned	uCols;

	uCols = pData->uColumnCount;
	if(uCols)
		uSize = pData->iColumnXpos[uCols]-1;
	else
		uSize = pData->iMaxSizeX;

	if (pData->uOldXCount==uSize)
		if (pData->uOldXPage ==pData->uSizeX) {
			return;
		}

	pData->uOldYPage	= pData->uSizeX;
	pData->uOldYCount	= uSize;

	UNLOCK(pData);

	sInfo.cbSize	= sizeof(SCROLLINFO);
	sInfo.fMask		= SIF_ALL;
	sInfo.nMin		= 0;
	sInfo.nMax  	= uSize;
	sInfo.nPage 	= pData->uSizeX;
	sInfo.nPos  	= pData->uScrollX;
	sInfo.nTrackPos	= 0;

	if (pData->uStyle&TVS_NOSCROLL) {
		sInfo.nMax = 0;
	} else if (pData->uStyleEx&TVS_EX_AUTOHSCROLL) {
		sInfo.nMax = 0;
	}

	if ((int)sInfo.nPage>=sInfo.nMax && pData->uScrollX>0) {
		sInfo.nPos      = 0;
		pData->uScrollX = 0;

		UpdateView(pData);

		if (pData->hHeader) {
			MoveWindow(pData->hHeader,0,0,pData->uSizeX-1,pData->uStartPixel,TRUE);
		}
	}

	SetScrollInfo(pData->hWnd,SB_HORZ,&sInfo,TRUE);

	LOCK(pData);
}

static void UpdateScrollY(TreeListData *pData)
{
	SCROLLINFO  sInfo;

	if (pData->uOldYCount==pData->uItemPosCount)
		if (pData->uOldYPage ==pData->uPageEnties) {
			return;
		}

	pData->uOldYPage	= pData->uPageEnties;
	pData->uOldYCount	= pData->uItemPosCount;

	UNLOCK(pData);

	sInfo.cbSize	= sizeof(SCROLLINFO);
	sInfo.fMask		= SIF_ALL;
	sInfo.nMin		= 0;
	sInfo.nMax  	= pData->uItemPosCount;
	sInfo.nPage 	= pData->uPageEnties;
	sInfo.nPos  	= pData->uScrollY;
	sInfo.nTrackPos	= 0;

	if (pData->uStyle&TVS_NOSCROLL) {
		sInfo.nMax = 0;
	}

	if ((int)sInfo.nPage>=sInfo.nMax && pData->uScrollY>0) {
		sInfo.nPos      = 0;
		pData->uScrollY = 0;
		
		UpdateView(pData);
	}

	SetScrollInfo(pData->hWnd,SB_VERT,&sInfo,TRUE);

	LOCK(pData);
}

static void UpdateToolTip(TreeListData *pData,size_t uItem,unsigned uFlags)
{
	HDC			hDc;
	HWND		hToolTip;
	ExtraItem  *pExtra;
	BaseItem   *pEntry;
	LPCWSTR	    pText;
	RECT		sRect;
	HFONT		hFont;
	unsigned	uSize;
	unsigned	uCol;
	int			iPixels;
	int			iTemp;

	if (!uItem) {
		if(pData->uToolTipItem)goto ExitTip;
		return;
	}

	pEntry=pData->pTreeItems[uItem];

	if (uFlags & TVHT_ONITEM) {
		if(pData->uToolTipItem != uItem || pData->uToolTipSub != 0) {
			TreeListGetItemRect(pData,uItem,TVIR_GETCOLUMN|TVIR_TEXT, &sRect);

			if (sRect.right-sRect.left <= pEntry->iTextPixels+4) {
				pText	= pEntry->pText;
				uSize	= pEntry->uTextSize;
				iPixels	= pEntry->iTextPixels;
				hFont	= (pEntry->uState&TVIS_BOLD) ? pData->hFontB
				                                     : pData->hFontN;

				if (pEntry->bCallback & TVIF_TEXT) {
					CallbackEntry(pData,pEntry,uItem,TVIF_TEXT,&iTemp,&uSize,&pText);
					hDc=GetDC    (pData->hWnd);
					SelectObject (hDc,hFont);
					DrawTextW    (hDc,pText,-1,&sRect,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX|DT_CALCRECT);
					ReleaseDC    (pData->hWnd,hDc);
					iPixels=sRect.right-sRect.left;
				}

				if (*pText==0)
					goto ExitTip;

				ClientToScreen(pData->hWnd,(LPPOINT)&sRect);

				hToolTip = pData->hToolTip;

				UNLOCK(pData);
				SendMessage  (hToolTip,WM_SETFONT,(WPARAM)hFont,0);
				SetWindowTextW(hToolTip,pText);
				SetWindowPos (hToolTip,HWND_TOP,sRect.left+1,sRect.top+1,iPixels+5,pData->iFontHeight+3,SWP_SHOWWINDOW|SWP_NOACTIVATE);
				RedrawWindow (hToolTip,NULL,NULL,RDW_INVALIDATE|RDW_UPDATENOW|RDW_ERASE|RDW_FRAME);
				LOCK(pData);

				pData->uToolTipItem = uItem;
				pData->uToolTipSub	= 0;

				SetTimer(pData->hWnd,ID_TOOLTIPCHECK,1500,NULL);
			} else {
				if(pData->uToolTipItem)goto ExitTip;
			}
		}

		return;
	}

	if (uFlags & (TVHT_ONSUBICON|TVHT_ONSUBLABEL)) {
		if (pData->uToolTipItem!=uItem || TVHT_SUBTOCOL(uFlags)!=pData->uToolTipSub) {
			uCol = TVHT_SUBTOCOL(uFlags);

			pExtra = pData->pExtraItems[uCol-1][uItem];

			if (pExtra) {
				TreeListGetItemRect(pData,uItem,TVIR_GETCOLUMN|TVIR_TEXT|TVIR_COLTOSUB(uCol),&sRect);
				if(sRect.right-sRect.left<=pExtra->iTextPixels+4) {
					pText	= pExtra->pText;
					uSize	= pExtra->uTextSize;
					iPixels	= pExtra->iTextPixels;
					hFont	=(pExtra->uState&TVIS_BOLD)? pData->hFontB:pData->hFontN;

					if (pExtra->bCallback&TVIF_TEXT) {
						CallbackExtra(pData,pEntry,pExtra,uItem,uCol,TVIF_TEXT,&iTemp,&uSize,&pText);
						hDc=GetDC    (pData->hWnd);
						SelectObject (hDc,hFont);
						DrawTextW    (hDc,pText,0,&sRect,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX|DT_CALCRECT);
						ReleaseDC    (pData->hWnd,hDc);
						iPixels=sRect.right-sRect.left;
					}

					if (*pText==0)
						goto ExitTip;

					ClientToScreen(pData->hWnd,(LPPOINT)&sRect);

					hToolTip = pData->hToolTip;

					UNLOCK(pData);
					SendMessage   (hToolTip,WM_SETFONT,(WPARAM)hFont,0);
					SetWindowTextW(hToolTip,pText);
					SetWindowPos  (hToolTip,HWND_TOP,sRect.left+1,sRect.top+1,iPixels+5,pData->iFontHeight+3,SWP_SHOWWINDOW|SWP_NOACTIVATE);
					RedrawWindow  (hToolTip,NULL,NULL,RDW_INVALIDATE|RDW_UPDATENOW|RDW_ERASE|RDW_FRAME);
					UNLOCK(pData);

					pData->uToolTipItem = uItem;
					pData->uToolTipSub	= uCol;

					SetTimer(pData->hWnd,ID_TOOLTIPCHECK,1500,NULL);
				} else {
					if (pData->uToolTipItem)
						goto ExitTip;
				}
			} else {
				if(pData->uToolTipItem)goto ExitTip;
			}
		}

		return;
	}

ExitTip:
	if(pData->uToolTipItem) {
		ShowWindow(pData->hToolTip,SW_HIDE);
		pData->uToolTipItem = 0;
		pData->uToolTipSub	= 0;

		KillTimer(pData->hWnd,ID_TOOLTIPCHECK);

		GetClientRect  (pData->hToolTip,&sRect);
		MapWindowPoints(pData->hToolTip,pData->hWnd,(LPPOINT)&sRect,2);
		InvalidateRect (pData->hWnd,&sRect,FALSE);
	}
}

static int UpdateFont(TreeListData *pData) 
{
	int			iPos;
	int			iRet;
	HDC			hDc;
	LOGFONT		sLog;
	TEXTMETRIC	sMetrics;
	BaseItem   *pEntry;
	BaseItem  **pList;
	ExtraItem  *pExtra;
	ExtraItem **pItems;
	unsigned	uSub;

	if (!hDefaultFontN) {
		SystemParametersInfo(SPI_GETICONTITLELOGFONT,sizeof(sLog),&sLog,0);
		sLog.lfWeight=FW_NORMAL;hDefaultFontN=CreateFontIndirect(&sLog);
		sLog.lfWeight=FW_BOLD  ;hDefaultFontB=CreateFontIndirect(&sLog);
	}

	if (!pData->hFontN)
		pData->hFontN = hDefaultFontN;

	if (pData->hFontN==hDefaultFontN) {
		pData->hFontB=hDefaultFontB;
	} else {
		pData->hFontB=pData->hFontN;
	}

	if (pData->hFontN!=pData->hFontL) {
		pData->hFontL=pData->hFontN;

		hDc = GetDC(NULL);
		SelectObject(hDc,pData->hFontN);
		GetTextMetrics(hDc,&sMetrics);
		pData->iFontHeight =  sMetrics.tmHeight;
		pData->iFontLine   =  sMetrics.tmAscent+1;
		pData->iFontOff    = (sMetrics.tmPitchAndFamily&TMPF_FIXED_PITCH)? 0:-1;
		ReleaseDC(NULL,hDc);

		pList = pData->pTreeItems;
		iPos  = pData->uTreeItemsMax;

		for (;iPos>=0;iPos--) {
			pEntry = pList[iPos];
			if (!pEntry)
				continue;

			pEntry->iTextPixels=0;
		}

		for (uSub=1;uSub<pData->uColumnCount;uSub++) {
			iPos    = pData->uTreeItemsMax;
			pItems 	= pData->pExtraItems[uSub-1];

			for(;iPos>=0;iPos--) {
				pExtra = pItems[iPos];
				if(!pExtra)continue;

				pExtra->iTextPixels=0;
			}
		}

		iRet=1;
	} else {
		iRet=0;
	}

	return iRet;
}

static void UpdateItems(TreeListData *pData,size_t uItem)
{
	unsigned	uPos;
	unsigned	uOld;
	unsigned	uNum;
	unsigned	uTemp;
	unsigned	uStart;
	unsigned   *pLines;
	BaseItem  **pItems;
	BaseItem   *pEntry;
	BaseItem   *pTemp;
	RECT		sRect;

	uOld	= pData->uItemPosCount;
	pLines	= pData->pItemPos;
	pItems	= pData->pTreeItems;

	if (!uItem) {
		uItem = pData->uFirstChild;
		if (!uItem) {
			if(!uOld)
				return;

			for(uNum=0;uNum<uOld;uNum++) {
				uTemp = pLines[uNum];
				if (!uTemp)
					continue;
				pLines[uNum]=0;
				pTemp = pItems[uTemp];
				if(!pTemp)continue;
					pTemp->uShowPos = 0;
			}

			memset(pLines,0,sizeof(unsigned)*uOld);
			return;
		}

		for (uNum=0;uNum<uOld;uNum++) {
			uTemp = pLines[uNum];
			if (!uTemp)
				continue;
			pLines[uNum]=0;
			pTemp = pItems[uTemp];
			if (!pTemp)
				continue;
			pTemp->uShowPos = 0;
		}

		pEntry			 = pItems[uItem];
		pEntry->uShowPos = 1;
		pLines[0]		 = uItem;
		uPos			 = 1;
		uStart			 = 0;
	} else {													
		pEntry			 = pItems[uItem];
		uPos			 = pEntry->uShowPos;
		if (uPos)
			uStart	 = uPos-1;
		else
			uStart	 = 0;

		for (uNum=uPos;uNum<uOld;uNum++)	{
			uTemp = pLines[uNum];
			if (!uTemp)
				continue;
			pLines[uNum]=0;
			pTemp = pItems[uTemp];
			if (!pTemp)
				continue;
			pTemp->uShowPos = 0;
		}
	}

	for(;;)	{
		if (pEntry->uFirstChild && (pEntry->uState&TVIS_EXPANDED)) {
			uItem=pEntry->uFirstChild;
		} else if(pEntry->uNextItem) {
			uItem=pEntry->uNextItem;
		} else {
			for(;;) {
				uItem = pEntry->uParent;
				if (!uItem)
					break;

				pEntry = pItems[uItem];
				if (pEntry->uNextItem) {
					uItem = pEntry->uNextItem;
					break;
				}
			}

			if (!uItem)
				break;
		}

		pEntry = pItems[uItem];

		if (pLines[uPos]!=uItem) {
			pLines[uPos] = uItem;
		} else {
			if (uStart==uPos)
				uStart++;
		}

		uPos++;
		pEntry->uShowPos = uPos;
	}

	pData->uItemPosCount = uPos;

	if (uStart>pData->uScrollY)
		uStart-= pData->uScrollY;
	else
		uStart = 0;

	GetClientRect (pData->hWnd,&sRect);
	sRect.top    = pData->uStartPixel-1;
	sRect.top    = pData->iRowHeight*uStart;
	InvalidateRect(pData->hWnd,&sRect,FALSE);

	if (uOld!=uPos)
		UpdateScrollY(pData);
}

static int UpdateColumns(TreeListData *pData)
{
	HWND		hHeader;
	RECT		sRect;
	unsigned	uPos;
	int			iNum;
	int			iRet=0x10000;

	hHeader = pData->hHeader;
	pData->iColumnXpos[0] = 0;

	for (uPos=0; uPos<pData->uColumnCount; ) {
		Header_GetItemRect(hHeader,uPos,&sRect);

		uPos++;
		iNum=pData->iColumnXpos[uPos];

		if (iNum == sRect.right)
			continue;
		if (iNum == 0) iNum=sRect.left;
		if (iNum >= sRect.right) {
			if (pData->cColumnAlign[uPos-1] == DT_LEFT)
				iRet = sRect.right;
			else
				iRet = sRect.left;
		} else {
			if (pData->cColumnAlign[uPos-1]==DT_LEFT)
				iRet = iNum;
			else
				iRet = sRect.left;

			if (pData->uSelectedItem && pData->uSelectedSub+1==uPos) {
				UpdateRect(pData,pData->uSelectedItem,pData->uSelectedSub);
			}

			if (pData->uTrackedItem  && pData->uTrackedSub+1==uPos) {
				UpdateRect(pData,pData->uTrackedItem,pData->uTrackedSub);
			}
		}

		pData->iColumnXpos[uPos] = sRect.right;
		break;
	}

	for (;uPos < pData->uColumnCount;) {
		Header_GetItemRect(hHeader,uPos,&sRect);

		uPos++;
		sRect.right--;
		pData->iColumnXpos[uPos] = sRect.right;
	}

	pData->iColumnXpos[pData->uColumnCount+1] = pData->uSizeX;

	return iRet;
}

static int TreeListToggleItem(TreeListData *pData,size_t uItem,unsigned uAddFlags)
{
	NMTREEVIEW	sNotify;
	BaseItem  **pList;
	BaseItem   *pEntry;
	BaseItem   *pTemp;
	unsigned	uLevel;
	unsigned	uMask;
	unsigned	uBits;
	unsigned	uNext;
	LRESULT		lRet;

	if (uItem>pData->uTreeItemsMax)
		return 0;

	pList  = pData->pTreeItems;
	pEntry = pList[uItem];
	if (!pEntry)
		return -1;

	UNLOCK(pData);

	uBits						= pEntry->uState&TVIS_EXPANDEDONCE;
	uMask						=(pEntry->uState&TVIS_EXPANDED)? ~(TVIS_EXPANDEDONCE|TVIS_EXPANDED):~0;
	sNotify.hdr.code			= TVN_ITEMEXPANDING;
	sNotify.action				= (pEntry->uState&TVIS_EXPANDED)? TVE_COLLAPSE:TVE_EXPAND;
	sNotify.itemNew.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE;
	sNotify.itemNew.hItem		= (HTREEITEM)uItem;
	sNotify.itemNew.stateMask	= 0xFFFFFFFF;
	sNotify.itemNew.state		=(pEntry->uState^TVIS_EXPANDED)&uMask;
	sNotify.itemNew.lParam		= pEntry->lParam;
	sNotify.itemNew.pszText		= (LPTSTR)-1;
	sNotify.itemNew.cchTextMax	= -1;
	sNotify.itemOld.mask		= 0;
	sNotify.ptDrag.x			= 0;
	sNotify.ptDrag.y			= 0;

	lRet=SendNotify(pData,&sNotify.hdr);

	LOCK(pData);

	pEntry = pList[uItem];
	if (!pEntry)
		return -1;

	if (lRet)
		return 1;										

	pList =  pData->pTreeItems;

	pEntry->uState ^=  TVIS_EXPANDED;					
	pEntry->uState &= ~TVIS_EXPANDPARTIAL;
	pEntry->uState |=  uAddFlags;
	if (pEntry->uShowPos) {
		if (pEntry->uState&TVIS_EXPANDED) {
			uLevel			=  0;
			uNext			=  pEntry->uFirstChild;

			while (uNext) {
				pTemp = pList[uNext];
				pTemp->uShowPos = 0;

				if (pTemp->uFirstChild) {
					uNext=pTemp->uFirstChild;
					uLevel++;
					continue;
				}

				if (pTemp->uNextItem) {
					uNext=pTemp->uNextItem;
					continue;
				}

				if (uLevel==0)
					break;

				uNext = pList[pTemp->uParent]->uNextItem;
				uLevel--;
			}
		}

		UpdateItems(pData,uItem);
	}

	UNLOCK(pData);

	sNotify.hdr.code			= TVN_ITEMEXPANDED;
	sNotify.action				= (pEntry->uState&TVIS_EXPANDED)? TVE_COLLAPSE:TVE_EXPAND;
	sNotify.itemNew.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE;
	sNotify.itemNew.hItem		= (HTREEITEM)uItem;
	sNotify.itemNew.stateMask	= 0xFFFFFFFF;
	sNotify.itemNew.state		=(pEntry->uState|uBits)&uMask;
	sNotify.itemNew.lParam		= pEntry->lParam;
	sNotify.itemNew.pszText		= (LPTSTR)-1;
	sNotify.itemNew.cchTextMax	= -1;
	sNotify.itemOld.mask		= 0;
	sNotify.ptDrag.x			= 0;
	sNotify.ptDrag.y			= 0;

	SendNotify(pData,&sNotify.hdr);

	LOCK(pData);

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry)
		return -1;

	if (pData->uSelectedItem) {
		pEntry = pList[pData->uSelectedItem];
		if (!pEntry->uShowPos) {
			while (!pEntry->uShowPos) {
				uItem  = pEntry->uParent;
				pEntry = pList[uItem];
			}

			TreeListSelectItem(pData,uItem,pData->uSelectedSub,TVC_UNKNOWN);
		}
	}

	return 0;
}

static int TreeListGetItemRect(TreeListData *pData,size_t uItem,unsigned uFlags,RECT *pRect)
{
	ExtraItem  *pExtra;
	BaseItem   *pEntry;
	unsigned	uPos;
	unsigned	uSub;

	if (uItem>pData->uTreeItemsMax) {
		memset(pRect,0,sizeof(RECT));
		return 0;
	}

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry->uShowPos) {
		memset(pRect,0,sizeof(RECT));
		return 0;
	}

	uPos = pEntry->uShowPos-pData->uScrollY-1;
	if (uPos>=pData->uMaxEnties) {
		memset(pRect,0,sizeof(RECT));
		return 0;
	}

	pRect->top     = pData->uStartPixel;
	pRect->top    += pData->iRowHeight*uPos;
	pRect->bottom  = pData->iRowHeight+pRect->top;

	if ((uFlags&0xFE)==TVIR_GETCOLUMN) {
		uSub = uFlags>>24;
		if (uSub>=pData->uColumnCount)
			uSub=0;

		pRect->left    = pData->iColumnXpos[uSub  ];
		pRect->left   -= pData->uScrollX;
		pRect->right   = pData->iColumnXpos[uSub+1];
		pRect->right  -= pData->uScrollX;
	} else {
		uSub		   = 0;
		pRect->left    = 0;
		pRect->left   -= pData->uScrollX;
		pRect->right   = pData->uSizeX;
	}

	if (uFlags&TVIR_TEXT) {
		if (uSub>0) {
			pExtra =  pData ->pExtraItems[uSub-1][uItem];
			if (pExtra && pExtra->bFlags&TVIX_HASIMAGE) {
				pRect->left+=pData->iImagesXsize;
				if (pRect->left>pRect->right)
					pRect->left=pRect->right;
			}
		} else {
			if (pData->uStyle&TVS_LINESATROOT) {
				pRect->left += pData->iIdent;
			}

			pRect->left += pData->iIdent*pEntry->uLevel;

			if (pData->hStates) {
				pRect->left += pData->iStatesXsize;
			}

			if (pData->uStyleEx&TVS_EX_ITEMLINES) {
				pRect->left += 1;
				if (pEntry->bFlags&TVIX_HASIMAGE)
					pRect->left++;
			}

			if (pEntry->bFlags&TVIX_HASIMAGE) {
				pRect->left += pData->iImagesXsize;
			}

			if (pRect->left>pRect->right)
				pRect->left=pRect->right;
		}
	}

	return 1;
}

static int TreeListEnsureVisible(TreeListData *pData,size_t uItem,unsigned uSub)
{
	BaseItem   *pEntry;
	BaseItem   *pTemp;
	unsigned	uTemp;
	unsigned	uPos;
	int			iNum;
	int			iAnf;
	int			iOff;
	int			iEnd;
	int			iMax;
	int			iRet;

	if (uItem>pData->uTreeItemsMax)
		return -1;

	pEntry = pData->pTreeItems[uItem];
	if(!pEntry)return -1;

	uPos = pEntry->uShowPos;
	if (!uPos) {
		iRet = 0;

		for (pTemp=pEntry;;) {
			uTemp = pTemp->uParent;
			pTemp = pData->pTreeItems[uTemp];
			if (!pTemp)
				break;
			if ((pTemp->uState&TVIS_EXPANDED)==0) {
				if (TreeListToggleItem(pData,uTemp,0))
					return 0;
			}
		}

		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;

		uPos = pEntry->uShowPos;
		if (!uPos)
			return 0;
	} else {
		iRet = 1;
	}

	uPos--;
	if (uPos<pData->uScrollY) {
		pData->uScrollY	= uPos;
		SetScrollPos(pData->hWnd,SB_VERT,uPos,TRUE);
		UpdateView(pData);
	} else if (uSub==FIRST_LINE) {
		if (uPos!=pData->uScrollY) {
			pData->uScrollY	= uPos;
			SetScrollPos(pData->hWnd,SB_VERT,uPos,TRUE);
			UpdateView(pData);
		}

		return iRet;
	} else if (uPos>=pData->uScrollY+pData->uPageEnties) {
		iOff  = uPos-(pData->uPageEnties-1);
		iMax  = pData->uItemPosCount;
		iMax -=	pData->uPageEnties-1;

		if (iOff>=iMax)
			iOff=iMax;
		if (iOff<0)
			iOff=0;
		if (iOff!=(int)pData->uScrollY) {
			pData->uScrollY	= iOff;
			SetScrollPos(pData->hWnd,SB_VERT,iOff,TRUE);
			UpdateView(pData);
		}
	}

	if (uSub<pData->uColumnCount) {
		iNum  = pData->uSizeX;
		iOff  = pData->uScrollX;
		iAnf  = pData->iColumnXpos[uSub  ];
		iEnd  = pData->iColumnXpos[uSub+1];

		if (iOff+iNum< iAnf) iOff=iAnf;
		if (iOff     >=iEnd) iOff=iAnf;
		if (iOff+iNum< iEnd) iOff=iEnd-iNum;
		if (iOff     > iAnf) iOff=iAnf;

		iMax  = pData->iColumnXpos[pData->uColumnCount];
		iMax -= pData->uSizeX/2;

		if (iOff>iMax)iOff=iMax;
		if (iOff<   0)iOff=0;
		if (iOff!=(int)pData->uScrollX) {
			pData->uScrollX	= iOff;
			SetScrollPos(pData->hWnd,SB_HORZ,iOff,TRUE);
			UpdateView(pData);
			MoveWindow(pData->hHeader,-iOff,0,iNum+iOff-1,pData->uStartPixel,TRUE);
		}
	}

	return iRet;
}

static int TreeListDeleteItem(TreeListData *pData,size_t uItem,int iUpdateList)
{
	NMTREEVIEW	sNotify;
	ExtraItem **pList;
	ExtraItem  *pExtra;
	BaseItem   *pEntry;
	BaseItem   *pTemp;
	unsigned	uPos;
	int			iOff;
	int			iMax;

	if (pData->cLockChanges)
		return 0;

	if (uItem>pData->uTreeItemsMax) {
		if (uItem != U(TVI_ROOT))
			return 0;					
		if (pData->uLastChild==0)
			return 0;

		while (pData->uLastChild) {
			TreeListDeleteItem(pData,pData->uLastChild,0);
		}

		if (iUpdateList)
			UpdateItems(pData,0);

		return 1;
	}

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry) {
		if (uItem!=0)
			return 0;							
		if (pData->uLastChild==0)
			return 0;

		while (pData->uLastChild) {
			TreeListDeleteItem(pData,pData->uLastChild,0);
		}

		if (iUpdateList)
			UpdateItems(pData,0);

		return 1;
	}

	while (pEntry->uLastChild) {
		TreeListDeleteItem(pData,pEntry->uLastChild,0);
	}

	sNotify.hdr.code			= TVN_DELETEITEM;
	sNotify.itemNew.mask		= 0;
	sNotify.itemOld.mask		= TVIF_HANDLE|TVIF_PARAM;
	sNotify.itemOld.hItem		= (HTREEITEM)uItem;
	sNotify.itemOld.lParam		= pEntry->lParam;
	sNotify.itemOld.pszText		= (LPTSTR)-1;
	sNotify.itemOld.cchTextMax	= -1;
	sNotify.ptDrag.x			= 0;
	sNotify.ptDrag.y			= 0;

	UNLOCK(pData);
	SendNotify(pData,&sNotify.hdr);
	LOCK(pData);

	pEntry = pData->pTreeItems[uItem];					
	if (!pEntry)
		return 0;

	if (pData->uInsertMark==uItem) {
		pData->uInsertMark = 0;
	}

	if (pEntry->uPrevItem) {
		pTemp			 = pData->pTreeItems[pEntry->uPrevItem];
		pTemp->uNextItem = pEntry->uNextItem;
	} else {
		if (pEntry->uParent) {
			pTemp			   = pData->pTreeItems[pEntry->uParent];
			pTemp->uFirstChild = pEntry->uNextItem;
		} else {
			pData->uFirstChild = pEntry->uNextItem;
		}
	}

	if (pEntry->uNextItem) {
		pTemp			 = pData->pTreeItems[pEntry->uNextItem];
		pTemp->uPrevItem = pEntry->uPrevItem;
	} else {
		if (pEntry->uParent) {
			pTemp			  = pData->pTreeItems[pEntry->uParent];
			pTemp->uLastChild = pEntry->uPrevItem;
		} else {
			pData->uLastChild = pEntry->uPrevItem;
		}
	}

	for (uPos=1;uPos<pData->uColumnCount;uPos++) {
		pList = pData->pExtraItems[uPos-1];

		pExtra = pList[uItem];
		if (!pExtra)
			continue;

		pList[uItem] = NULL;

		if (pExtra->pText) {
			pExtra->uTextSize=0;
			free(pExtra->pText);
		}

		free(pExtra);
	}

	pData->pTreeItems[uItem] = NULL;						

	if (pEntry->pText) {
		pEntry->uTextSize=0;
		free(pEntry->pText);
	}

	if (iUpdateList) {
		uItem = pEntry->uPrevItem;
		if(!uItem)uItem=pEntry->uParent;

		if(pEntry->uShowPos) {
			UpdateItems(pData,uItem);
		}
	}

	free(pEntry);

	pData->uTreeItemsCount--;


	if(uItem==pData->uSelectedItem) {						
		pData->uSelectedItem = 0;
		pData->uSelectedSub  = 0;
	}

	if(uItem==pData->uTrackedItem) {						
		pData->uTrackedItem  = 0;
		pData->uTrackedSub   = 0;
	}

	iOff  = pData->uScrollY;								
	iMax  = pData->uItemPosCount;
	iMax -=	pData->uPageEnties-1;

	if(iOff>=iMax)iOff=iMax;
	if(iOff<0)iOff=0;
	if(iOff!=(int)pData->uScrollY) {
		pData->uScrollY	= iOff;
		SetScrollPos(pData->hWnd,SB_VERT,iOff,TRUE);
		UpdateView(pData);
	}

	return 1;
}

static int TreeListXorSelectItem(TreeListData *pData,size_t uItem,int iMode)
{
	NMTREEVIEW	sNotify;
	BaseItem   *pEntry;
	unsigned	uRet;

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry)
		return 0;
	if (uItem==pData->uSelectedItem)
		return 0;

	sNotify.hdr.code			= TVN_SELCHANGING;
	sNotify.action				= iMode;
	sNotify.itemNew.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBITEM;
	sNotify.itemNew.hItem		= (HTREEITEM)uItem;
	sNotify.itemNew.stateMask	= 0xFFFFFFFF;
	sNotify.itemNew.state		= pEntry->uState^TVIS_SELECTED;
	sNotify.itemNew.cChildren	= 0;
	sNotify.itemNew.pszText		= (LPTSTR)-1;
	sNotify.itemNew.cchTextMax	= -1;
	sNotify.itemOld.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBITEM;
	sNotify.itemOld.hItem		= NULL;
	sNotify.itemOld.stateMask	= 0xFFFFFFFF;
	sNotify.itemOld.state		= 0;
	sNotify.itemOld.cChildren	= 0;
	sNotify.itemOld.pszText		= (LPTSTR)-1;
	sNotify.itemOld.cchTextMax	= -1;
	sNotify.ptDrag.x			= 0;
	sNotify.ptDrag.y			= 0;

	UNLOCK(pData);
	uRet=SendNotify(pData,&sNotify.hdr);
	LOCK(pData);

	if (uRet)
		return 0;

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry)
		return 0;

	sNotify.hdr.code			= TVN_SELCHANGED;
	sNotify.action				= iMode;
	sNotify.itemNew.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBITEM;
	sNotify.itemNew.hItem		= (HTREEITEM)uItem;
	sNotify.itemNew.stateMask	= 0xFFFFFFFF;
	sNotify.itemNew.state		= pEntry->uState^TVIS_SELECTED;
	sNotify.itemNew.cChildren	= 0;
	sNotify.itemNew.pszText		= (LPTSTR)-1;
	sNotify.itemNew.cchTextMax	= -1;
	sNotify.itemOld.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBITEM;
	sNotify.itemOld.hItem		= NULL;
	sNotify.itemOld.stateMask	= 0xFFFFFFFF;
	sNotify.itemOld.state		= 0;
	sNotify.itemOld.cChildren	= 0;
	sNotify.itemOld.pszText		= (LPTSTR)-1;
	sNotify.itemOld.cchTextMax	= -1;
	sNotify.ptDrag.x			= 0;
	sNotify.ptDrag.y			= 0;

	UNLOCK(pData);
	SendNotify(pData,&sNotify.hdr);
	LOCK(pData);

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry)
		return 0;
	pEntry->uState ^= TVIS_SELECTED;
	if (pEntry->uShowPos) {
		if(pData->uStyleEx&TVS_EX_FULLROWMARK)
			UpdateRow (pData,uItem);
		else
			UpdateRect(pData,uItem,0);
	}

	if (pEntry->uState&TVIS_SELECTED)
		pData->uSelectedCount++;
	else
		pData->uSelectedCount--;

	return 1;
}

static void TreeListRemoveFocus(TreeListData *pData)
{
	ExtraItem  *pExtra;
	BaseItem   *pEntry;
	unsigned	uItem;
	unsigned	uSub;

	if (!pData->uFocusItem)
		return;

	uItem  = pData->uFocusItem;
	pEntry = pData->pTreeItems[uItem];

	if (!pEntry)
		return;

	pEntry->bFlags &= ~TVIX_FOCUSED;

	uSub = pData->uFocusSub;

	if (uSub) {
		pExtra= pData->pExtraItems[uSub-1][uItem];
		if (pExtra)
			pExtra->bFlags &= ~TVIX_FOCUSED;
	}

	UpdateRect(pData,uItem,uSub);

	pData->uFocusItem = 0;
	pData->uFocusSub  = 0;
}

static int TreeListSelectItem(TreeListData *pData,size_t uItem,size_t uSubItem,int iMode)
{
	NMTREEVIEW	sNotify;
	ExtraItem  *pExtra;
	BaseItem   *pEntry;
	BaseItem   *pTemp;
	unsigned	uState;
	unsigned	uStOld;
	unsigned	uNext;
	unsigned	uPos;
	size_t		uOld;
	unsigned	uSub;
	unsigned	uRet;
	int			iDel;

	uOld = pData->uSelectedItem;
	uSub = pData->uSelectedSub;

	if (uSubItem>=pData->uColumnCount && uSubItem>0)
		return 0;
	if (uItem   > pData->uTreeItemsMax)
		return 0;
	if (uItem   ==uOld)
		if (uSubItem==uSub)
			return 1;

	if (pData->uStyleEx&TVS_EX_MULTISELECT) {
		iDel = iMode&TVC_DESELECT;
		if (!iDel) {
			if(pData->uStyleEx&TVS_EX_FULLROWMARK)
				UpdateRow (pData,uOld);
			else
				UpdateRect(pData,uOld,uSub);

			uOld = 0;
			uSub = 0;
		} else {												
			pData->uSelectedBase = uItem;

			if (pData->uSelectedCount>1 && pData->uTreeItemsMax) {
				for (uPos=pData->uTreeItemsMax;uPos;uPos--) {
					pEntry = pData->pTreeItems[uPos];
					if (!pEntry || !(pEntry->uState&TVIS_SELECTED))
						continue;
					if (TreeListXorSelectItem(pData,uPos,iMode))
						if (!pData->uSelectedCount)
							break;
				}
			}
		}
	} else {
		iDel = 1;
	}

	iMode &= ~TVC_DESELECT;

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry) {
		if (uItem)
			return 0;
		uState = 0;
	} else {
		uState = pEntry->uState|TVIS_SELECTED;

		if (uSubItem) {
			uState &= TVIS_BASEFLAGS;
			pExtra  = pData->pExtraItems[uSubItem-1][uItem];
			if (pExtra)
				uState|=pExtra->uState;
		}
	}

	pTemp = pData->pTreeItems[uOld];
	if (!pTemp)	{
		uStOld = 0;
	} else {
		uStOld = pTemp->uState|TVIS_SELECTED;

		if (uSub) {
			uStOld &= TVIS_BASEFLAGS;
			pExtra  = pData->pExtraItems[uSub-1][uOld];
			if (pExtra)
				uStOld|=pExtra->uState;
		}
	}

	sNotify.hdr.code			= TVN_SELCHANGING;
	sNotify.action				= iMode;
	sNotify.itemNew.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBITEM;
	sNotify.itemNew.hItem		= (HTREEITEM)uItem;
	sNotify.itemNew.stateMask	= 0xFFFFFFFF;
	sNotify.itemNew.state		= uState;
	sNotify.itemNew.cChildren	= uSubItem;
	sNotify.itemNew.pszText		= (LPTSTR)-1;
	sNotify.itemNew.cchTextMax	= -1;
	sNotify.itemOld.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBITEM;
	sNotify.itemOld.hItem		= (HTREEITEM)uOld;
	sNotify.itemOld.stateMask	= 0xFFFFFFFF;
	sNotify.itemOld.state		= uStOld;
	sNotify.itemOld.cChildren	= uSub;
	sNotify.itemOld.pszText		= (LPTSTR)-1;
	sNotify.itemOld.cchTextMax	= -1;
	sNotify.ptDrag.x			= 0;
	sNotify.ptDrag.y			= 0;

	UNLOCK(pData);

	if (SendNotify(pData,&sNotify.hdr))	{
		LOCK(pData);
		return 0;
	}

	LOCK(pData);

	if (uItem) {
		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;
	}

	if (iDel) {
		uOld    = pData->uSelectedItem;
		pTemp	= pData->pTreeItems[uOld];
	}

	if (pTemp) {
		if(pTemp->uShowPos) {
			if(pData->uStyleEx&TVS_EX_FULLROWMARK)
				UpdateRow (pData,uOld);
			else
				UpdateRect(pData,uOld,uSub);
		}

		if (pTemp->uState&TVIS_SELECTED) {
			pTemp->uState &= ~TVIS_SELECTED;
			pData->uSelectedCount -= 1;
		}

		pData->uSelectedSub    = 0;
		pData->uSelectedItem   = 0;
	} else {
		uOld  = 0;
	}

	if (uItem) {
		if (uSubItem && uSubItem<pData->uColumnCount) {
			pExtra = pData->pExtraItems[uSubItem-1][uItem];
			if(!pExtra) {
				pExtra  = malloc(sizeof(ExtraItem));
				memset(pExtra,0,sizeof(ExtraItem));
				pExtra->iImage = TV_NOIMAGE;
				pExtra->uState = pEntry->uState&(TVIS_BOLD|TVIS_UNTERLINE);
				pData->pExtraItems[uSubItem-1][uItem] = pExtra;
			}

			uState  = pExtra->uState;
			uState |= pEntry->uState&TVIS_BASEFLAGS;
		} else {
			uState  = pEntry->uState;
		}

		if (pEntry->uShowPos) {
			if (pData->uStyleEx&TVS_EX_FULLROWMARK)
				UpdateRow (pData,uItem);
			else
				UpdateRect(pData,uItem,uSubItem);
		}

		if (!(pEntry->uState&TVIS_SELECTED)) {
			pEntry->uState |= TVIS_SELECTED;
			pData->uSelectedCount += 1;
		}

		pData->uSelectedSub    = uSubItem;
		pData->uSelectedItem   = uItem;
	} else{
		pData->uSelectedItem = 0;
		pData->uSelectedSub  = 0;
		uState				 = 0;
	}

	sNotify.hdr.code			= TVN_SELCHANGED;
	sNotify.action				= iMode;
	sNotify.itemNew.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBITEM;
	sNotify.itemNew.hItem		= (HTREEITEM)uItem;
	sNotify.itemNew.stateMask	= 0xFFFFFFFF;
	sNotify.itemNew.state		= uState;
	sNotify.itemNew.cChildren	= uSubItem;
	sNotify.itemNew.pszText		= (LPTSTR)-1;
	sNotify.itemNew.cchTextMax	= -1;
	sNotify.itemOld.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBITEM;
	sNotify.itemOld.hItem		= (HTREEITEM)uOld;
	sNotify.itemOld.stateMask	= 0xFFFFFFFF;
	sNotify.itemOld.state		= uStOld;
	sNotify.itemOld.cChildren	= uSub;
	sNotify.itemOld.pszText		= (LPTSTR)-1;
	sNotify.itemOld.cchTextMax	= -1;
	sNotify.ptDrag.x			= 0;
	sNotify.ptDrag.y			= 0;

	UNLOCK(pData);
	SendNotify(pData,&sNotify.hdr);
	LOCK(pData);

	if (!(pData->uStyle&TVS_SINGLEEXPAND)) {
		if (pData->uStyle&TVS_SHOWSELALWAYS)
			if(pData->uSelectedItem) {
				TreeListEnsureVisible(pData,pData->uSelectedItem,pData->uSelectedSub);
			}

		return 1;
	}

	sNotify.hdr.code			= TVN_SINGLEEXPAND;
	sNotify.action				= iMode;
	sNotify.itemNew.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE;
	sNotify.itemNew.hItem		= (HTREEITEM)uItem;
	sNotify.itemNew.stateMask	= 0xFFFFFFFF;
	sNotify.itemNew.state		= (pEntry)? pEntry->uState:0;
	sNotify.itemNew.cChildren	= 0;
	sNotify.itemNew.pszText		= (LPTSTR)-1;
	sNotify.itemNew.cchTextMax	= -1;
	sNotify.itemOld.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE;
	sNotify.itemOld.hItem		= (HTREEITEM)uOld;
	sNotify.itemOld.stateMask	= 0xFFFFFFFF;
	sNotify.itemOld.state		= (pTemp)? pTemp->uState:0;
	sNotify.itemOld.cChildren	= 0;
	sNotify.itemOld.pszText		= (LPTSTR)-1;
	sNotify.itemOld.cchTextMax	= -1;
	sNotify.ptDrag.x			= 0;
	sNotify.ptDrag.y			= 0;

	UNLOCK(pData);
	uRet=SendNotify(pData,&sNotify.hdr);					
	LOCK(pData);

	pTemp	= pData->pTreeItems[uOld ];						
	pEntry	= pData->pTreeItems[uItem];

	while (pTemp && pEntry)	{
		if (pEntry->uLevel>pTemp->uLevel) {
			uNext  = pEntry->uParent;

			if (!(uRet&TVNRET_SKIPNEW))
			if (!(pEntry->uState&TVIS_EXPANDED)) {
				TreeListToggleItem(pData,uItem,0);
			}

			pEntry = pData->pTreeItems[uNext];
			uItem  = uNext;

			if (!uItem)
				break;

			continue;
		}

		if (uItem==uOld)
			goto EndSel;							

		uNext  = pTemp->uParent;

		if (!(uRet&TVNRET_SKIPOLD))
			if (pTemp->uState&TVIS_EXPANDED) {
				TreeListToggleItem(pData,uOld,0);
			}

		pTemp  = pData->pTreeItems[uNext];
		uOld   = uNext;
	}

	if (!uItem) {
		if (!(uRet&TVNRET_SKIPOLD))
			while(pTemp) {
				uNext = pTemp->uParent;

				if(pTemp->uState&TVIS_EXPANDED) {
					TreeListToggleItem(pData,uOld,0);
				}

				pTemp = pData->pTreeItems[uNext];
				uOld  = uNext;
			}

		goto EndSel;
	}

	if (!uOld) {
		if (!(uRet&TVNRET_SKIPNEW)) 
			while(pEntry) {
				uNext = pEntry->uParent;

				if (!(pEntry->uState&TVIS_EXPANDED)) {
					TreeListToggleItem(pData,uItem,0);
				}

				pEntry = pData->pTreeItems[uNext];
				uItem  = uNext;
			}
	}

EndSel:
	if(pData->uStyle&TVS_SHOWSELALWAYS)
		if (pData->uSelectedItem) {
			TreeListEnsureVisible(pData,pData->uSelectedItem,pData->uSelectedSub);
		}

	return 2;
}

static unsigned TreeListInsertItem(TreeListData *pData, TV_INSERTSTRUCTW *pInsert)
{
	char	   *pTemp;
	LPCWSTR	    pText;
	LPCWSTR	    pTextTemp;
	ExtraItem **pExOld[MAX_COLUMNS];
	ExtraItem **pExNew[MAX_COLUMNS];
	BaseItem   *pNew;
	BaseItem  **pOld;
	BaseItem  **pItems;
	BaseItem   *pEntry;
	BaseItem   *pParent;
	unsigned   *pPosNew;
	unsigned   *pPosOld;
	size_t     *pFirst;
	size_t	   *pLast;
	unsigned	uParent;
	unsigned	uSize;
	unsigned	uBits;
	unsigned	uItem;
	unsigned	uMax;
	unsigned	uPos;
	unsigned	uNum;
	int			iNone;

	if (pData->cLockChanges) return 0;

	uParent=U(pInsert->hParent);
	if(uParent>pData->uTreeItemsMax) {
		if (pInsert->hParent!=TVI_ROOT) {
			return 0;
		}

		pParent = NULL;
	} else {
		pParent = pData->pTreeItems[uParent];
		if(!pParent) {
			if (uParent) return 0;
			pParent = NULL;
		}
	}

	if (pData->uTreeItemsCount+1>pData->uTreeItemsMax) {
		pPosOld = pData->pItemPos;
		pOld	= pData->pTreeItems;
		uMax	= pData->uTreeItemsMax;
		uMax   += pData->uTreeItemsMax/2;
		uMax   += 64;
		pItems  = malloc(sizeof(BaseItem*)*(uMax+1));

		if(!pItems) {
			return 0;
		}

		pPosNew = malloc(sizeof(unsigned)*uMax);
		if(!pPosNew) {
			free(pItems);
			return 0;
		}

		for (uPos=1;uPos<pData->uColumnCount;uPos++) {
			pExOld[uPos] = pData->pExtraItems[uPos-1];
			pExNew[uPos] = malloc(sizeof(ExtraItem*)*(uMax+1));

			if (!pExNew[uPos]) {
				for(uPos--;uPos>0;uPos--)
					free(pExNew[uPos]);
				free(pPosNew);
				free(pItems);
				return 0;
			}
		}

		memcpy(pItems ,pData->pTreeItems       ,sizeof(BaseItem*)*(     pData->uTreeItemsMax+1));
		memset(pItems +pData->uTreeItemsMax+1,0,sizeof(BaseItem*)*(uMax-pData->uTreeItemsMax  ));
		memcpy(pPosNew,pData->pItemPos         ,sizeof(unsigned )*(     pData->uTreeItemsCount));
		memset(pPosNew+pData->uTreeItemsCount,0,sizeof(unsigned )*(uMax-pData->uTreeItemsCount));

		for (uPos=1;uPos<pData->uColumnCount;uPos++) {
			memcpy(pExNew[uPos],pExOld[uPos]            ,sizeof(ExtraItem*)*(     pData->uTreeItemsMax+1));
			memset(pExNew[uPos]+pData->uTreeItemsMax+1,0,sizeof(ExtraItem*)*(uMax-pData->uTreeItemsMax  ));
			pData->pExtraItems[uPos-1]=pExNew[uPos];
			free(pExOld[uPos]);
		}

		pData->uTreeItemsMax = uMax;
		pData->pTreeItems	 = pItems;
		pData->pItemPos		 = pPosNew;
		free(pPosOld);
		free(pOld);
	}

	pItems	= pData->pTreeItems;
	uPos	= pData->uNextSeachPos+1;
	pTemp	= malloc(sizeof(BaseItem)+pData->uUserDataSize);
	pNew	= (BaseItem*)pTemp;

	if(pData->uUserDataSize) {								
		memset(pTemp+sizeof(BaseItem),0,pData->uUserDataSize);
	}

	if(!pNew)
	{
		return 0;
	}

	for(;;uPos++) {											
		if(uPos>pData->uTreeItemsMax)uPos=1;
		if(pItems[uPos]==NULL)break;
	}

	pData->uNextSeachPos = uPos;

	memset(pNew,0,sizeof(BaseItem));						

	uBits = pInsert->item.mask;

	if (uBits & TVIF_STATE) {
		pNew->uState = pInsert->item.state & pInsert->item.stateMask;
	} else {
		if (pData->uStyle&TVS_CHECKBOXES)
			pNew->uState=0x1000;
		}

		if (uBits&TVIF_PARAM) {
			pNew->lParam = pInsert->item.lParam;
		}

		if (uBits & TVIF_IMAGE) {
			pNew->iImage = pInsert->item.iImage;
			if (pNew->iImage==I_IMAGECALLBACK)
				pNew->bCallback |= TVIF_IMAGE;
		}

		if (uBits & TVIF_SELECTEDIMAGE) {
			pNew->iSelectedImage = pInsert->item.iSelectedImage;
			if (pNew->iSelectedImage==I_IMAGECALLBACK)
				pNew->bCallback|=TVIF_SELECTEDIMAGE;
		}

		if (uBits&TVIF_CHILDREN) {
			switch (pInsert->item.cChildren)
			{
			case  0:											break;
			case  1:		pNew->bFlags   |=TVIX_HASBUTTON;	break;
			case  I_CCB:	pNew->bCallback|=TVIF_CHILDREN;		break;
			default:		pNew->bFlags   |=TVIX_VARBUTTON;	break;
			}
		} else {
			pNew->bFlags |=TVIX_VARBUTTON;
		}

		if(pData->uStyle&TVS_SINGLEEXPAND) {
			pNew->uState &= ~TVIS_EXPANDED;
		}

		pNew->uState |= TVIS_EXPANDEDONCE;

		if (uBits & TVIF_TEXT) {
			if (pInsert->item.pszText==LPSTR_TEXTCALLBACKW) {
				pNew->bCallback|= TVIF_TEXT;
				pNew->uTextSize = 0;
				pNew->pText		= 0;
			} else {
				pNew->uTextSize	= (WORD)pslen(pInsert->item.pszText);
				pNew->pText		= malloc(sizeof(WCHAR)*(pNew->uTextSize+1));
				memcpy(pNew->pText,pInsert->item.pszText,sizeof(WCHAR)*(pNew->uTextSize+1));
			}
		} else {
			pNew->pText		= malloc(sizeof(WCHAR));
			pNew->pText[0]	= 0;
			pNew->uTextSize	= 0;
		}

		if (!pParent) {
			pNew->uParent	= 0;
			uParent			= 0;
			pFirst			= &pData->uFirstChild;
			pLast			= &pData->uLastChild;
		} else {													
			pNew->uParent	=  uParent;
			pNew->uLevel	=  pParent->uLevel+1;
			pFirst			= &pParent->uFirstChild;
			pLast			= &pParent->uLastChild;

			if (pParent->bFlags&TVIX_VARBUTTON) {
				pParent->bFlags|=TVIX_HASBUTTON;
			}
		}

		switch (U(pInsert->hInsertAfter)) {
		case U(TVI_FIRST):
			if (pFirst[0]) {
				pEntry = pItems[pFirst[0]];
				pEntry->uPrevItem = uPos;
			} else {
				pFirst[0] = uPos;
				pLast [0] = uPos;
				break;
			}

			pNew ->uNextItem = pFirst[0];					
			pFirst[0]		 = uPos;

			break;

		case U(TVI_ROOT):
			pNew->uParent	= 0;
			uParent			= 0;
			pFirst			= &pData->uFirstChild;
			pLast			= &pData->uLastChild;

		case U(TVI_LAST):
			if (pLast[0]) {
				pEntry = pItems[pLast[0]];
				pEntry->uNextItem = uPos;
			} else {
				pFirst[0] = uPos;
				pLast [0] = uPos;
				break;
			}

			pNew ->uPrevItem = pLast[0];					
			pLast[0]		 = uPos;
			break;

	case U(TVI_SORT):
			uItem = pFirst[0];
			if (!uItem) {
				pFirst[0] = uPos;
				pLast [0] = uPos;
				break;
			}

			pData->cLockChanges=1;

			for (;uItem;uItem=pItems[uItem]->uNextItem) {
				pEntry = pItems[uItem];
				if (pEntry->bCallback&TVIF_TEXT) {
					uSize=0;
					LOCK(pData);
					CallbackEntry(pData,pEntry,uItem,TVIF_TEXT,&iNone,&uSize,&pTextTemp);
					UNLOCK(pData);
				} else {
					pTextTemp = pEntry->pText;
				}

				if (pNew->bCallback & TVIF_TEXT) {
					uSize=1;
					LOCK(pData);
					CallbackEntry(pData,pEntry,uPos,TVIF_TEXT,&iNone,&uSize,&pText);
					UNLOCK(pData);
				} else {
					pText = pEntry->pText;
				}

				if (pscmp(pNew->pText,(PortString) pTextTemp)<0)
					break;
			}

			pData->cLockChanges=0;

			if (!uItem) {
				pEntry = pItems[pLast[0]];
				pEntry->uNextItem = uPos;
				pNew  ->uPrevItem = pLast[0];
				pLast[0]		  = uPos;
				break;
			}

			if(uItem==pFirst[0]) {
				pFirst[0]=uPos;
			}

			pEntry = pItems[uItem];
			pEntry->uPrevItem = uPos;
			pNew  ->uNextItem = uItem;

			break;

 	case U(TVI_ROW):
		uItem = pFirst[0];
		if (!uItem) {
			pFirst[0] = uPos;
			pLast [0] = uPos;
			break;
		}

		uNum=0;

		for (;uItem;uItem=pItems[uItem]->uNextItem) {
			if (uNum==U(pInsert->hInsertAfter)) {
				uItem=pItems[uItem]->uNextItem;
				break;
			}

			uNum++;
		}

		if (!uItem) {
			pEntry = pItems[pLast[0]];
			pEntry->uNextItem = uPos;
			pNew  ->uPrevItem = pLast[0];
			pLast[0]		  = uPos;
			break;
		}

		if (uItem==pFirst[0]) {
			pFirst[0]=uPos;
		}

		pEntry = pItems[uItem];
		pEntry->uPrevItem = uPos;
		pNew  ->uNextItem = uItem;

		break;

 	default:												
		uItem = pFirst[0];
		if (!uItem)	{
			pFirst[0] = uPos;
			pLast [0] = uPos;
			break;
		}

		for (;uItem;uItem=pItems[uItem]->uNextItem)	{
			if (uItem==U(pInsert->hInsertAfter)) {
				uItem=pItems[uItem]->uNextItem;
				break;
			}
		}

		if (!uItem)	{
			pEntry = pItems[pLast[0]];
			pEntry->uNextItem = uPos;
			pNew  ->uPrevItem = pLast[0];
			pLast[0]		  = uPos;
			break;
		}

		if (uItem==pFirst[0]) {
			pFirst[0]=uPos;
		}

		pEntry = pItems[uItem];
		pEntry->uPrevItem = uPos;
		pNew  ->uNextItem = uItem;

		break;
	}


	pItems[uPos] = pNew;
	pData->uTreeItemsCount++;

	if (!pParent || (pParent->uState&TVIS_EXPANDED)) {
		uItem = pNew->uPrevItem;
		if (!uItem)
			uItem = uParent;

		if (!uItem)
			UpdateItems(pData,0);
		else {
			pEntry = pItems[uItem];
			if (pEntry && pEntry->uShowPos)
				UpdateItems(pData,0);
		}
	}

	if (pNew->uState&TVIS_SELECTED) {
		TreeListSelectItem(pData,uPos,0,TVC_UNKNOWN);
	}

	return uPos;
}

static int TreeListSetItem(TreeListData *pData,const TVITEMW *pItem)
{
	BYTE			bCall;
	BYTE			bFlags;
	ExtraItem	  **pList;
	ExtraItem	   *pExtra;
	BaseItem	   *pEntry;
	unsigned		uChange;
	unsigned		uMask;
	unsigned		uBits;
	unsigned		uItem;
	unsigned		uSub;
	unsigned		uLen;
	int				iVal;
	int				iRet;

	uChange = 0;

	uItem = U(pItem->hItem);
	if (uItem>pData->uTreeItemsMax) return -1;

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry)
		return -1;

	uBits = pItem->mask;
	if (uBits & TVIF_SUBITEM) {
		uSub = pItem->cChildren;
		if (uSub > 0) {
			if (uSub >= pData->uColumnCount)
				return -1;
			pList	= pData->pExtraItems[uSub-1];
			pExtra	= pList[uItem];

			if (!pExtra) {
				pExtra  = malloc(sizeof(ExtraItem));
				memset(pExtra,0,sizeof(ExtraItem));
				pExtra->iImage = TV_NOIMAGE;
				pExtra->uState = pEntry->uState&(TVIS_BOLD|TVIS_UNTERLINE);
				pList[uItem]   = pExtra;
			}

			if (uBits & TVIF_PARAM) {
				pEntry->lParam	= pItem->lParam;
			}

			if ((uBits & TVIF_IMAGE) && pExtra->iImage!=pItem->iImage) {
				if (pData->hImages)
					uChange=1;
				pExtra->iImage = pItem->iImage;
				if (pExtra->iImage==I_IMAGECALLBACK)
					pExtra->bCallback |= TVIF_IMAGE;
				else
					pExtra->bCallback &= TVIF_IMAGE;
			}

			if (uBits & TVIF_TEXT) {
				if (pItem->pszText==LPSTR_TEXTCALLBACKW) {
					if (pExtra->pText) free(pExtra->pText);
					pExtra->bCallback |= TVIF_TEXT;
					pExtra->uTextSize = 0;
					pExtra->pText	  = 0;
					uChange			  = 1;
				} else {
					uLen = pslen(pItem->pszText);

					if (uLen>pExtra->uTextSize || !pExtra->pText) {
						if(pExtra->pText) free(pExtra->pText);
						pExtra->pText=malloc(sizeof(WCHAR)*(uLen+1));
					}

					memcpy(pExtra->pText,pItem->pszText,(uLen+1)*sizeof(WCHAR));
					pExtra->bCallback  &=~TVIF_TEXT;
					pExtra->uTextSize   = (WORD)uLen;
					pExtra->iTextPixels = 0;
					uChange=1;
				}
			}
				
			if (uBits&TVIF_STATE) {
				uMask			=  pItem->stateMask&~TVIS_BASEFLAGS;
				uBits			=  uMask&(pExtra->uState^pItem->state);
				uBits		   |= (pItem->stateMask& TVIS_BASEFLAGS)&(pEntry->uState^pItem->state);
				pExtra->uState &= ~uMask;
				pExtra->uState |=  uMask&pItem->state;

				if ((uBits&TVIS_OVERLAYMASK|TVIS_CUT) && pData->hImages) {
					uChange=1;
				}

				if (uBits & (TVIS_BOLD|TVIS_DROPHILITED)) {
					pExtra->iTextPixels=0;
					uChange=1;
				}

				if ((uBits & TVIS_EXPANDED) && pEntry->uFirstChild) {
					iVal=TreeListToggleItem(pData,uItem,0);
					if (iVal<0)
						return -1;

					pEntry = pData->pTreeItems[uItem];
					if (!pEntry)
						return -1;
				}

				if (uBits & TVIS_SELECTED) {
					iVal = (pData->uStyleEx&TVS_EX_SUBSELECT)? uSub:0;

					if (pItem->state&TVIS_SELECTED)
						iRet = TreeListSelectItem(pData,uItem,iVal,TVC_UNKNOWN);
					else
						iRet = TreeListSelectItem(pData,0    ,0   ,TVC_UNKNOWN);

					pEntry = pData->pTreeItems[uItem];
					if (!pEntry)
						return -1;

					if (iRet>=2) {
						pList	= pData->pExtraItems[uSub-1];
						pExtra	= pList[uItem];
						if (!pExtra)
							return -1;
					} else if (iRet==1) {
						uChange=1;
					}
				}
			}

			if(!uChange || !pEntry->uShowPos)return 0;		

			UpdateRect(pData,uItem,uSub);

			return 0;
		}

		uBits &= ~TVIF_CHILDREN;
	}

	if (uBits&TVIF_PARAM) {
		pEntry->lParam	= pItem->lParam;
	}

	if ((uBits&TVIF_IMAGE) && pEntry->iImage!=pItem->iImage) {
		pEntry->iImage = pItem->iImage;
		if (!(pEntry->uState&TVIS_SELECTED) && pData->hImages)
			uChange=1;
		if (  pEntry->iImage==I_IMAGECALLBACK)
			pEntry->bCallback |= TVIF_IMAGE;
		else
			pEntry->bCallback &= TVIF_IMAGE;
	}

	if ((uBits & TVIF_SELECTEDIMAGE) && pEntry->iSelectedImage!=pItem->iSelectedImage) {
		pEntry->iSelectedImage = pItem->iSelectedImage;
		if ((pEntry->uState&TVIS_SELECTED) && pData->hImages)
			uChange=1;
		if (pEntry->iSelectedImage == I_IMAGECALLBACK)
			pEntry->bCallback |= TVIF_SELECTEDIMAGE;
		else
			pEntry->bCallback &= TVIF_SELECTEDIMAGE;
	}

	if (uBits & TVIF_CHILDREN) {
		bCall  = pEntry->bCallback;
		bFlags = pEntry->bFlags;

		switch (pItem->cChildren) {
		case  0:
			pEntry->bCallback &= ~TVIF_CHILDREN;
			pEntry->bFlags    &= ~TVIX_HASBUTTON;
			pEntry->bFlags    &=  TVIX_VARBUTTON;
			break;

		case  1:
			pEntry->bCallback &= ~TVIF_CHILDREN;
			pEntry->bFlags    &=  TVIX_VARBUTTON;
			pEntry->bFlags    |=  TVIX_HASBUTTON;
			break;

		case  I_CCB:
			pEntry->bCallback |=  TVIF_CHILDREN;
			pEntry->bFlags    &=  TVIX_VARBUTTON;
			break;

		default:
			pEntry->bCallback &= ~TVIF_CHILDREN;
			pEntry->bFlags    |=  TVIX_VARBUTTON;

			if (pEntry->uFirstChild)
				pEntry->bFlags |= TVIX_HASBUTTON;
			else
				pEntry->bFlags &=~TVIX_HASBUTTON;
		}

		if (bCall!=pEntry->bCallback || bFlags!=pEntry->bFlags) {
			uChange=1;
		}
	}

	if (uBits & TVIF_TEXT) {
		if (pItem->pszText==LPSTR_TEXTCALLBACKW) {
			if (pEntry->pText)
				free(pEntry->pText);
			pEntry->bCallback|= TVIF_TEXT;
			pEntry->uTextSize = 0;
			pEntry->pText	  = 0;
			uChange			  = 1;
		} else {
			uLen = pslen(pItem->pszText);

			if (uLen>pEntry->uTextSize) {
				if (pEntry->pText)
					free(pEntry->pText);
				pEntry->pText=malloc(sizeof(WCHAR)*(uLen+1));
			}

			memcpy(pEntry->pText,pItem->pszText,(uLen+1)*sizeof(WCHAR));
			pEntry->bCallback  &=~TVIF_TEXT;
			pEntry->uTextSize   = (WORD)uLen;
			pEntry->iTextPixels = 0;
			uChange=1;
		}
	}

	if (uBits & TVIF_STATE) {
		uMask = pItem->stateMask;

		if (pData->uStyle&TVS_SINGLEEXPAND)	{
			uMask &= ~TVIS_EXPANDED;
		}

		uBits			=  uMask&(pEntry->uState^pItem->state);
		pEntry->uState &= ~uMask;
		pEntry->uState |=  uMask&pItem->state;

		if ((uBits&(TVIS_OVERLAYMASK|TVIS_CUT)) && pData->hImages) {
			uChange=1;
		}

		if ((uBits&TVIS_STATEIMAGEMASK) && pData->hStates) {
			uChange=1;
		}

		if (uBits&(TVIS_BOLD|TVIS_DROPHILITED)) {
			pEntry->iTextPixels = 0;
			uChange=1;
		}

		if (uBits&TVIS_SELECTED) {
			pEntry->uState ^= TVIS_SELECTED;

			if (pItem->state&TVIS_SELECTED)
				iRet = TreeListSelectItem(pData,uItem,0,TVC_UNKNOWN);
			else
				iRet = TreeListSelectItem(pData,0    ,0,TVC_UNKNOWN);

			pEntry = pData->pTreeItems[uItem];
			if (!pEntry)
				return -1;

			if (iRet==1) {
				uChange=1;
			}
		}

		if ((uBits&TVIS_EXPANDED) && pEntry->uFirstChild) {
			pEntry->uState^= TVIS_EXPANDED;

			iRet=TreeListToggleItem(pData,uItem,0);
			if (iRet<1)
				return iRet;
		}
	}

	if (uChange && pEntry->uShowPos) {
		UpdateRect(pData,uItem,0);
	}

	return 0;
}

static unsigned TreeListGetItem(TreeListData *pData,TV_ITEMW *pItem)
{
	ExtraItem	  **pList;
	ExtraItem	   *pExtra;
	BaseItem	   *pEntry;
	unsigned		uBits;
	unsigned		uItem;
	unsigned		uSub;
	unsigned		uLen;

	uItem = U(pItem->hItem);
	if (uItem>pData->uTreeItemsMax) return 0;

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry) return 0;


	uBits = pItem->mask;

	if (uBits & TVIF_SUBITEM) {
		uSub = pItem->cChildren;
		if(uSub>0) {
			if (uSub>= pData->uColumnCount)
				return 0;
			pList	= pData->pExtraItems[uSub-1];
			pExtra	= pList[uItem];

			if (!pExtra) {
				pExtra  = malloc(sizeof(ExtraItem));
				memset(pExtra,0,sizeof(ExtraItem));
				pExtra->iImage = TV_NOIMAGE;
				pExtra->uState = pEntry->uState&(TVIS_BOLD|TVIS_UNTERLINE);
				pList[uItem]   = pExtra;
			}

			if (uBits&TVIF_PARAM) {
				pItem->lParam = pEntry->lParam;
			}

			if (uBits&TVIF_IMAGE) {
				pItem->iImage = pExtra->iImage;
			}

			if (uBits&TVIF_TEXT) {
				if (pExtra->pText==LPSTR_TEXTCALLBACKW) {
					pItem->pszText = LPSTR_TEXTCALLBACKW;
				} else if (uBits & TVIF_TEXTPTR) {
					if (pExtra->pText) {
						pItem->pszText    = L"";
						pItem->cchTextMax = 0;
					} else {
						pItem->pszText    = pExtra->pText;
						pItem->cchTextMax = pExtra->uTextSize+1;
					}
				} else {
					if (pExtra->pText) {
						uLen = pExtra->uTextSize+1;
						if (pItem->cchTextMax<(int)uLen) {
							uLen=(pItem->cchTextMax<=0)? 0:pItem->cchTextMax;
						}

						memcpy(pItem->pszText,pExtra->pText,uLen*sizeof(WCHAR));
					} else {
						pItem->pszText[0]=0;
					}
				}
			}

			if (uBits&TVIF_STATE) {
				pItem->state = pExtra->uState&~(TVIS_EXPANDED|TVIS_EXPANDEDONCE);
			}

			return 1;
		}

		if(pEntry->bCallback&TVIF_CHILDREN)
			pItem->cChildren = (pEntry->uFirstChild)? 1:0;
		else
			pItem->cChildren = I_CHILDRENCALLBACK;

		uBits &= ~TVIF_CHILDREN;
	}

	if (uBits & TVIF_PARAM) {
		pItem->lParam = pEntry->lParam;
	}

	if (uBits & TVIF_IMAGE) {
		pItem->iImage = pEntry->iImage;
	}

	if (uBits&TVIF_SELECTEDIMAGE) {
		pItem->iSelectedImage = pEntry->iSelectedImage;
	}

	if (uBits & TVIF_CHILDREN) {
		if (pEntry->bCallback&TVIF_CHILDREN)
			pItem->cChildren = (pEntry->uFirstChild)? 1:0;
		else
			pItem->cChildren = I_CHILDRENCALLBACK;
	}

	if (uBits & TVIF_TEXT) {
		if (pEntry->pText==LPSTR_TEXTCALLBACKW) {
			pItem->pszText = LPSTR_TEXTCALLBACKW;
		} else if (uBits&TVIF_TEXTPTR) {
			pItem->pszText    = pEntry->pText;
			pItem->cchTextMax = pEntry->uTextSize+1;
		} else {
			uLen = pEntry->uTextSize+1;
			if (pItem->cchTextMax<(int)uLen) {
				uLen=(pItem->cchTextMax<=0)? 0:pItem->cchTextMax;
			}

			memcpy(pItem->pszText,pEntry->pText,uLen*sizeof(WCHAR));
		}
	}

	if (uBits & TVIF_STATE) {
		pItem->state = pEntry->uState;
	}

	return 1;
}

static int TreeListDeleteColumn(TreeListData *pData,unsigned uCol)
{
	ExtraItem **pList;
	ExtraItem  *pExtra;
	HDITEMW		sItem;
	RECT		sRect;
	int			iSize;
	int			iXoff;
	int			iNum;
	int			iVar=0;
	unsigned	uItem;
	unsigned	uPos;

	if (uCol>=pData->uColumnCount)
		return 0;

	if (uCol && uCol==pData->uSelectedSub) {
		TreeListSelectItem(pData,pData->uSelectedItem,0,TVC_UNKNOWN);
	}

	if (uCol && uCol==pData->uEditSub) {
		pData->uEditSub  = 0;
		pData->uEditItem = 0;
		TreeListEndLabelEdit(pData,1);
	}

	if (uCol && uCol==pData->uFocusSub) {
		pData->uFocusSub  = 0;
		pData->uFocusItem = 0;
	}

	if (uCol==pData->uTrackedSub) {
		pData->uTrackedSub  = 0;
		pData->uTrackedItem = 0;
	}

	GetClientRect(pData->hWnd,&sRect);

	iSize = pData->uSizeX;

	for (uPos=0;uPos<pData->uColumnCount;uPos++) {
		if(pData->cColumnVar[uPos]) {
			iVar++;
			continue;
		}
		if (uPos==uCol)
			continue;
		iSize-=pData->iColumnXpos[uPos+1]-pData->iColumnXpos[uPos];
	}

	Header_DeleteItem(pData->hHeader,uCol);
	pData->uColumnCount--;

	if (pData->uColumnCount>0) {
		iNum=uCol-1;
		if (iNum<0)
			iNum=0;

		pList = pData->pExtraItems[uCol];
		if(pList) {
			for(uItem=0;uItem<=pData->uTreeItemsMax;uItem++) {
				pExtra = pList[uItem];
				if(!pExtra)continue;

				if(pExtra->pText) {
					pExtra->uTextSize=0;
					free(pExtra->pText);
				}

				free(pExtra);
			}

			memmove(pData->pExtraItems+iNum,pData->pExtraItems+iNum+1,sizeof(pList)*(MAX_COLUMNS-1-iNum));
			pData->pExtraItems[pData->uColumnCount]=NULL;
			free(pList);
		}
	} else {
		iNum=MAX_COLUMNS;
	}

	if (pData->cColumnVar[uCol]) {
		pData->uColumnCountVar--;
	}

	memmove(pData->cColumnAlign+uCol,pData->cColumnAlign+uCol+1,MAX_COLUMNS-1-uCol);
	memmove(pData->cColumnVar  +uCol,pData->cColumnVar  +uCol+1,MAX_COLUMNS-1-uCol);
	pData->cColumnVar[pData->uColumnCount]=0;

	if (pData->uColumnCountVar>0) {
		sItem.mask	= HDI_WIDTH;
		iNum		= pData->uColumnCountVar;

		for(uPos=0;uPos<pData->uColumnCount;uPos++) {
			if(!pData->cColumnVar[uPos])continue;
			sItem.cxy   = iSize/iNum;
			Header_SetItemW(pData->hHeader,uPos,&sItem);
			iSize	   -= sItem.cxy;
			iNum	   -= 1;
		}
	}

	if (!pData->uColumnCount) {
		DestroyWindow(pData->hHeader);
		pData->hHeader	   = NULL;
		pData->uStartPixel = 0;
		InvalidateRect(pData->hWnd,&sRect,FALSE);
	}

	iXoff=UpdateColumns(pData);							
	if (iXoff<0x10000) {
		sRect.left  = iXoff;
		sRect.left -= pData->uScrollX;
		sRect.top   = pData->uStartPixel;
		InvalidateRect(pData->hWnd,&sRect,FALSE);
	}

	UpdateScrollX(pData);
	return 1;
}

static int TreeListInsertColumn(TreeListData *pData,int iCol,TVCOLUMNW *pColumn)
{
	ExtraItem **pList;
	HDITEMW	 	sItem;
	RECT		sRect;
	int			iSize;
	int			iXoff;
	int			iYoff;
	int			iNum;
	int			iAdd=0;
	int			iVar=0;
	char		cAlign;
	char 		cFlag;
	unsigned	uPos;

	GetClientRect(pData->hWnd,&sRect);

	if (!pData->hHeader) {
		iYoff = sRect.top+GetSystemMetrics(SM_CYHSCROLL);

		pData->hHeader=CreateWindowW(WC_HEADERW,NULL,WS_VISIBLE|WS_CHILD|HDS_HORZ|HDS_BUTTONS,sRect.left,sRect.top,sRect.right,iYoff,pData->hWnd,NULL,ghModule,NULL);
		if (!pData->hHeader)
			return -1;

		pData->uStartPixel = GetSystemMetrics(SM_CYHSCROLL);

		InvalidateRect(pData->hWnd,&sRect,FALSE);
		SendMessage   (pData->hHeader,HDM_SETIMAGELIST,0,(LPARAM)pData->hImages);
		SendMessage   (pData->hHeader,WM_SETFONT,(WPARAM)hDefaultFontN,0);
		SetWindowLongPtrW(pData->hHeader,GWLP_ID,1);

		if (pData->uSizeX<=pData->uStartPixel)
			pData->uSizeYsub = 0;
		else
			pData->uSizeYsub = pData->uSizeX-pData->uStartPixel;
	}

	if (pData->uColumnCount>=MAX_COLUMNS) {
		return -1;
	}

	memset(&sItem,0,sizeof(sItem));							

	iSize = pData->uSizeX;

	for (uPos=0; uPos<pData->uColumnCount; uPos++) {
		if (pData->cColumnVar[uPos]) {
			iVar++;
			continue;
		}
		iSize-=pData->iColumnXpos[uPos+1]-pData->iColumnXpos[uPos];
	}

	if (pColumn->mask & TVCF_FMT) {
		sItem.mask    |= HDI_FORMAT;
		sItem.fmt      = pColumn->fmt;

		switch(sItem.fmt & HDF_JUSTIFYMASK) {
		case HDF_CENTER: cAlign = DT_CENTER;	break;
		case HDF_RIGHT:	 cAlign = DT_RIGHT;		break;
		default: 		 cAlign = DT_LEFT;		break;
		}
	} else {
		cAlign = DT_LEFT;
	}

	if (pColumn->mask&TVCF_IMAGE) {
		sItem.mask    |= HDI_IMAGE;
		sItem.iImage   = pColumn->iImage;
	}

	if (pColumn->mask &TVCF_TEXT) {
		sItem.mask    |= HDI_TEXT;
		sItem.pszText = pColumn->pszText;
	}

	if (pColumn->mask&TVCF_WIDTH) {
		if (iVar) {
			iAdd  =iSize%iVar;
			iSize/=iVar;
		}

		if (iSize<16) iSize=16;

		sItem.mask    |= HDI_WIDTH;
		sItem.cxy	   = pColumn->cx;
		cFlag		   = 0;
	} else {													
		iVar++;

		iAdd  =iSize%iVar;
		iSize /= iVar;
		if (iSize<16) iSize=16;

		sItem.mask    |= HDI_WIDTH;
		sItem.cxy      = iSize;
		cFlag		   = 1;
	}

    iCol = Header_InsertItemW(pData->hHeader,iCol,&sItem);
	if (iCol<0) return -1;

	if (pData->uColumnCount>0) {
		pList = malloc(sizeof(ExtraItem)*(pData->uTreeItemsMax+1));
		if (!pList) {
			Header_DeleteItem(pData->hHeader,iCol);
			return -1;
		}

		memset(pList,0,sizeof(ExtraItem*)*(pData->uTreeItemsMax+1));

		iNum = iCol-1;
		if (iNum<0)iNum=0;

		memmove(pData->pExtraItems+iNum+1,pData->pExtraItems+iNum,sizeof(pList)*(MAX_COLUMNS-2-iNum));
		pData->pExtraItems[iNum]=pList;
	}

	memmove(pData->cColumnAlign+iCol+1,pData->cColumnAlign+iCol,MAX_COLUMNS-1-iCol);
	memmove(pData->cColumnVar  +iCol+1,pData->cColumnVar  +iCol,MAX_COLUMNS-1-iCol);
	pData->cColumnAlign[iCol]   = cAlign;
	pData->cColumnVar  [iCol]   = cFlag;
	pData->uColumnCountVar     += cFlag;
	pData->uColumnCount	       += 1;

	if (pData->uSelectedSub>0 && pData->uSelectedSub-1<=(unsigned)iCol) {
		pData->uSelectedSub++;
	}

	if (pData->uTrackedSub>0 && pData->uTrackedSub-1<=(unsigned)iCol) {
		pData->uTrackedSub++;
	}

	iNum = iVar-cFlag;
	if (iNum>0)	{
		sItem.mask	= HDI_WIDTH;
		sItem.cxy   = iSize;

		for(uPos=0;uPos<pData->uColumnCount;uPos++) {
			if (!pData->cColumnVar[uPos]) continue;
			if ( uPos==(unsigned)iCol) continue;
			iNum--;
			if (!iNum) sItem.cxy+=iAdd;
			Header_SetItemW(pData->hHeader,uPos,&sItem);
		}
	}

	iXoff=UpdateColumns(pData);							
	if (iXoff<0x10000) {
		sRect.left  = iXoff;
		sRect.left -= pData->uScrollX;
		sRect.top   = pData->uStartPixel;
		InvalidateRect(pData->hWnd,&sRect,FALSE);
	}

	UpdateScrollX(pData);

	if (pData->uInsertMark) {
		TV_ITEMW	sSet;
		ExtraItem  *pExtra;

		sSet.mask		= TVIF_SUBITEM;
		sSet.hItem		= (HTREEITEM)pData->uInsertMark;
		sSet.cChildren	= iCol;

		TreeListSetItem(pData,&sSet);

		pExtra = pData->pExtraItems[iCol-1][pData->uInsertMark];
		if(pExtra) {
			pExtra->uColorBk  = pData->uColors[TVC_INSERT];
			pExtra->bFlags	 |= TVIX_BKCOLOR;
		}
	}

	return iCol;
}

static int TreeListScanColumn(TreeListData *pData,unsigned uSub)
{
	BaseItem  **pList;
	BaseItem   *pEntry;
	ExtraItem  *pExtra;
	ExtraItem **pItems;
	unsigned   *pPList;
	unsigned	uPos;
	int			iMax;
	int			iPos;


	if (uSub>=pData->uColumnCount)
		return 0;

	if (uSub>0)	{
		pItems = pData->pExtraItems[uSub-1];
		pPList = pData->pItemPos;
		iMax   = 0;

		for(uPos=0;uPos<pData->uItemPosCount;uPos++) {
			pExtra = pItems[pPList[uPos]];
			if(!pExtra) {
				if(iMax<8)iMax=8;
				continue;
			}

			if(pData->hImages && (pExtra->bFlags&TVIX_HASIMAGE))
				iPos = pData->iImagesXsize;
			else
				iPos = 0;

			iPos += pExtra->iTextPixels+8;
			if(iPos>iMax)iMax=iPos;
		}

		return iMax;
	}

	pList  = pData->pTreeItems;
	pPList = pData->pItemPos;
	iMax   = 0;

	for(uPos=0;uPos<pData->uItemPosCount;uPos++) {
		pEntry = pList[pPList[uPos]];

		if(pEntry->bFlags&TVIX_HASIMAGE)
			iPos = pData->iImagesXsize;
		else
			iPos = 0;

		iPos += pEntry->uLevel*pData->iIdent;
		iPos += pEntry->iTextPixels+8;
		if(iPos>iMax)iMax=iPos;
	}

	if (pData->uStyleEx&TVS_EX_ITEMLINES) iMax++;
	if (pData->uStyle  &TVS_LINESATROOT ) iMax+=pData->iIdent;
	if (pData->hStates) {
		iMax += pData->iStatesXsize;
	}

	return iMax;
}

static unsigned TreeListHitTest(TreeListData *pData,TV_HITTESTINFO *pInfo)
{
	int			iXpos;
	int			iYpos;
	int			iZpos;
	int			iWidth;
	int			iIcon;
	BaseItem   *pEntry;
	ExtraItem  *pExtra;
	size_t  	uItem;
	unsigned	uSub;

	iXpos = pInfo->pt.x;
	iYpos = pInfo->pt.y;

	if ((unsigned)iXpos>=pData->uSizeX) {
		pInfo->hItem = NULL;
		pInfo->flags = (iXpos<0)? TVHT_TOLEFT:TVHT_TORIGHT;
		return 0;
	}

	iYpos-=pData->uStartPixel;

	if ((unsigned)iYpos>=pData->uSizeY) {
		pInfo->hItem = NULL;
		pInfo->flags = (iYpos<0)? TVHT_ABOVE:TVHT_BELOW;
		return 0;
	}

	iZpos  = iYpos/pData->iRowHeight;
	iZpos +=       pData->uScrollY;

	if ((unsigned)iZpos>=pData->uItemPosCount) {
		pInfo->hItem = NULL;
		pInfo->flags = TVHT_NOWHERE;
		return 0;
	}

	iXpos		+= pData->uScrollX;
	uItem		 = pData->pItemPos  [iZpos];
	pEntry		 = pData->pTreeItems[uItem];
	pInfo->hItem = (HTREEITEM)uItem;

	if (!pEntry)
		return 0;

	if (iXpos>=pData->iColumnXpos[1]) {
		for (uSub=1;uSub<pData->uColumnCount;uSub++) {
			if (iXpos>=pData->iColumnXpos[uSub+1])
				continue;
			iXpos-=pData->iColumnXpos[uSub];

			pExtra = pData->pExtraItems[uSub-1][uItem];

			if (pExtra && (pExtra->bFlags&TVIX_HASIMAGE))
				iIcon = pData->iImagesXsize;
			else
				iIcon = 0;

			pInfo->flags = uSub<<24;

			if (iXpos<iIcon) {
				pInfo->flags |= TVHT_ONSUBICON;
				return uItem;
			}

			if (!pExtra || !pExtra->uTextSize) {
				pInfo->flags |= TVHT_ONSUBLABEL;
				return uItem;
			}

			switch (pData->cColumnAlign[uSub]) {
			default:
				if (iXpos-iIcon<pExtra->iTextPixels+5) {
					pInfo->flags |= TVHT_ONSUBLABEL;
					return uItem;
				}
				break;
			
			case DT_RIGHT:
				iWidth  = pData->iColumnXpos[uSub+1];
				iWidth -= pData->iColumnXpos[uSub  ];
			
				if(iXpos>=iWidth-pExtra->iTextPixels-5)	{
					pInfo->flags |= TVHT_ONSUBLABEL;
					return uItem;
				}
				break;
			
			case DT_CENTER:
				iWidth  = pData->iColumnXpos[uSub+1];
				iWidth -= pData->iColumnXpos[uSub  ];
				iWidth += iIcon;
				iWidth /= 2;
			
				if (iXpos>=iWidth-pExtra->iTextPixels/2-3)
					if (iXpos<=iWidth+pExtra->iTextPixels/2+3) {
						pInfo->flags |= TVHT_ONSUBLABEL;
						return uItem;
					}
					break;
			}

			pInfo->flags |= TVHT_ONSUBRIGHT;
			return uItem;
		}

		pInfo->flags = TVHT_ONRIGHTSPACE;

		return uItem;
	}


	if (!(pData->uStyle&TVS_LINESATROOT)) {
		iXpos += pData->iIdent;
	}

	iXpos -= pData->iIdent*pEntry->uLevel;

	if (iXpos<pData->iIdent) {
		if (pData->uStyle &TVS_HASBUTTONS)
			if (pEntry->bFlags&TVIX_HASBUTTON) {
				if(iXpos>=pData->iShift-6)
					if(iXpos<=pData->iShift+7) {
						iYpos %= pData->iRowHeight;
						iYpos -= pData->iRowHeight/2;

						if (iYpos>=-6 && iYpos<=7) {
							pInfo->flags = TVHT_ONITEMBUTTON;
							return uItem;
						}
					}
				}

				pInfo->flags = TVHT_ONITEMINDENT;
				return uItem;
	}

	iXpos -= pData->iIdent;

	if (pData->uStyleEx&TVS_EX_ITEMLINES) {
		iXpos--;
	}

	if (pData->hStates)	{
		iXpos -= pData->iStatesXsize;

		if (iXpos<0) {
			pInfo->flags = TVHT_ONITEMSTATEICON;
			return uItem;
		}
	}

	if (pEntry->bFlags & TVIX_HASIMAGE) {
		iXpos -= pData->iImagesXsize;

		if (pData->uStyleEx&TVS_EX_ITEMLINES)
			iXpos--;

		if (iXpos<0) {
			pInfo->flags = TVHT_ONITEMICON;
			return uItem;
		}
	}

	if (iXpos<pEntry->iTextPixels+5) {
		pInfo->flags = TVHT_ONITEMLABEL;
	} else {
		pInfo->flags = TVHT_ONITEMRIGHT;
	}

	return uItem;
}

static unsigned TreeListSetInsertMark(TreeListData *pData,size_t uItem,int iMode)
{
	TV_INSERTSTRUCTW sInsert;
	ExtraItem   *pExtra;
	BaseItem   *pEntry;
	unsigned	uSub;
	int	iRet;

	if (pData->uInsertMark) {
		iRet=TreeListDeleteItem(pData,pData->uInsertMark,1);
		pData->uInsertMark=0;
	} else {
		iRet=0;
	}

	if (uItem==0)
		return iRet;
	if (uItem>pData->uTreeItemsMax)
		return 0;

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry) return 0;

	if (iMode) {
		uItem=pEntry->uPrevItem;
		if (!uItem)
			uItem = U(TVI_FIRST);
	}

	sInsert.hParent		 		= (HTREEITEM)pEntry->uParent;
	sInsert.hInsertAfter 		= (HTREEITEM)uItem;
	sInsert.item.mask	 		= TVIF_SELECTEDIMAGE|TVIF_IMAGE;
	sInsert.item.iImage			= TV_NOIMAGE;
	sInsert.item.iSelectedImage	= TV_NOIMAGE;

	uItem = TreeListInsertItem(pData,&sInsert);
	if (!uItem)
		return 0;

	pEntry				= pData->pTreeItems[uItem];
	pEntry->uColorBk	= pData->uColors[TVC_INSERT];
	pEntry->bFlags	   |= TVIX_BKCOLOR;
	sInsert.item.mask  |= TVIF_SUBITEM;
	sInsert.item.hItem	= (HTREEITEM)uItem;

	for (uSub=1;uSub<pData->uColumnCount;uSub++) {
		sInsert.item.cChildren	= uSub;

		TreeListSetItem(pData,&sInsert.item);

		pExtra = pData->pExtraItems[uSub-1][uItem];
		pExtra->uColorBk	= pData->uColors[TVC_INSERT];
		pExtra->bFlags	   |= TVIX_BKCOLOR;
	}

	pData->uInsertMark = uItem;

	return uItem;
}

static COLORREF TreeListGetItemColor(TreeListData *pData,size_t uItem,unsigned uSub,int iMode)
{
	COLORREF	uOld;
	ExtraItem  *pExtra;
	BaseItem   *pEntry;

	if (uItem>pData->uTreeItemsMax)
		return TV_NOCOLOR;

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry)
		return TV_NOCOLOR;

	if (uSub) {
		if (uSub>=pData->uColumnCount)
			return TV_NOCOLOR;

		pExtra = pData->pExtraItems[uSub-1][uItem];
		if (!pExtra)
			return TV_NOCOLOR;

		if(iMode)
			uOld = (pExtra->bFlags&TVIX_TEXTCOLOR)? pExtra->uColorText:TV_NOCOLOR;
		else
			uOld = (pExtra->bFlags&TVIX_BKCOLOR  )? pExtra->uColorBk  :TV_NOCOLOR;
	} else {
		if (iMode)
			uOld = (pEntry->bFlags&TVIX_TEXTCOLOR)? pEntry->uColorText:TV_NOCOLOR;
		else
			uOld = (pEntry->bFlags&TVIX_BKCOLOR  )? pEntry->uColorBk  :TV_NOCOLOR;
	}

	return uOld;
}


static COLORREF TreeListSetItemColor(TreeListData *pData,size_t uItem,unsigned uSub,COLORREF uColor,int iMode)
{
	TV_ITEMW   sSet;
	COLORREF   uOld;
	ExtraItem *pExtra;
	BaseItem  *pEntry;

	if (uItem>pData->uTreeItemsMax)
		return TV_NOCOLOR;

	pEntry = pData->pTreeItems[uItem];
	if (!pEntry)
		return TV_NOCOLOR;

	if (uSub) {
		if (uSub>=pData->uColumnCount)
			return TV_NOCOLOR;

		pExtra = pData->pExtraItems[uSub-1][uItem];
		if (!pExtra) {
			sSet.mask		= TVIF_SUBITEM;
			sSet.hItem		= (HTREEITEM)uItem;
			sSet.cChildren	= uSub;

			if (TreeListSetItem(pData,&sSet))
				return TV_NOCOLOR;

			pExtra = pData->pExtraItems[uSub-1][uItem];
		}

		if (iMode) {
			uOld = (pExtra->bFlags&TVIX_TEXTCOLOR)? pExtra->uColorText:TV_NOCOLOR;

			if (uColor==TV_NOCOLOR) {
				pExtra->bFlags	   &= ~TVIX_TEXTCOLOR;
			} else {
				pExtra->bFlags	   |=  TVIX_TEXTCOLOR;
				pExtra->uColorText  =  uColor;
			}
		} else {												
			uOld = (pExtra->bFlags&TVIX_BKCOLOR)? pExtra->uColorBk:TV_NOCOLOR;

			if (uColor==TV_NOCOLOR) {
				pExtra->bFlags	   &= ~TVIX_BKCOLOR;
			} else {
				pExtra->bFlags	   |=  TVIX_BKCOLOR;
				pExtra->uColorBk    =  uColor;
			}
		}
	} else {
		if (iMode) {
			uOld = (pEntry->bFlags&TVIX_TEXTCOLOR) ? pEntry->uColorText
			                                       : TV_NOCOLOR;

			if (uColor==TV_NOCOLOR) {
				pEntry->bFlags	   &= ~TVIX_TEXTCOLOR;
			} else {
				pEntry->bFlags	   |=  TVIX_TEXTCOLOR;
				pEntry->uColorText  =  uColor;
			}
		} else {												
			uOld = (pEntry->bFlags&TVIX_BKCOLOR)? pEntry->uColorBk:TV_NOCOLOR;

			if (uColor==TV_NOCOLOR) {
				pEntry->bFlags	   &= ~TVIX_BKCOLOR;
			} else {
				pEntry->bFlags	   |=  TVIX_BKCOLOR;
				pEntry->uColorBk    =  uColor;
			}
		}
	}

	if (uColor!=uOld) {
		UpdateRect(pData,uItem,uSub);
	}

	return uOld;
}

static int TreeListSetTrackItem(TreeListData *pData,size_t uItem,unsigned uSub)
{
	ExtraItem	   *pExtra;
	BaseItem	   *pEntry;
	int				iRet=1;

	if (!(pData->uStyleEx&TVS_EX_SUBSELECT)) {
		uSub=0;
	} else {
		if (uSub>=pData->uColumnCount) {
			uItem = 0;
			uSub  = 0;
			iRet  = 0;
		}
	}

	if (uItem>pData->uTreeItemsMax) {
		uItem = 0;
		uSub  = 0;
		iRet  = 0;
	} else {
		if (uItem==pData->uTrackedItem)
			if (uSub ==pData->uTrackedSub) {
				return iRet;
			}
	}

	if (pData->uTrackedItem) {
		if (pData->uTrackedSub) {
			pExtra = pData->pExtraItems[pData->uTrackedSub-1][pData->uTrackedItem];
			if (pExtra) {
				pExtra->bFlags &= ~TVIX_TRACKED;
				UpdateRect(pData,pData->uTrackedItem,pData->uTrackedSub);
			}
		} else {
			pEntry = pData->pTreeItems[pData->uTrackedItem];
			if (pEntry) {
				pEntry->bFlags &= ~TVIX_TRACKED;
				UpdateRect(pData,pData->uTrackedItem,0);
			}
		}
	}

	if (uItem) {
		if (uSub) {
			pExtra = pData->pExtraItems[uSub-1][uItem];
			if (pExtra) {
				pData->uTrackedSub  = uSub;
				pData->uTrackedItem = uItem;
				pExtra->bFlags     |= TVIX_TRACKED;
				UpdateRect(pData,uItem,uSub);
			} else {
				iRet=0;
			}
		} else {
			pEntry = pData->pTreeItems[uItem];
			if (pEntry) {
				pData->uTrackedSub  = 0;
				pData->uTrackedItem = uItem;
				pEntry->bFlags     |= TVIX_TRACKED;
				UpdateRect(pData,uItem,0);
			} else {
				iRet=0;
			}
		}
	}

	return iRet;
}

static unsigned TreeListGetNextItem(TreeListData *pData,size_t uItem,unsigned uFlags)
{
	BaseItem	   *pEntry;
	unsigned		uStop;
	unsigned		uPos;

	switch (uFlags) {
	case TVGN_ROOT:
		return pData->uFirstChild;

	case TVGN_NEXT:
		if (uItem>pData->uTreeItemsMax) {
			return 0;
		}

		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;

		return pEntry->uNextItem;

	case TVGN_PREVIOUS:
		if (uItem>pData->uTreeItemsMax) {
			return 0;
		}

		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;

		return pEntry->uPrevItem;

	case TVGN_PARENT:
		if (uItem>pData->uTreeItemsMax) {
			return 0;
		}

		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;

		return pEntry->uParent;

	case TVGN_CHILD:
		if (uItem>pData->uTreeItemsMax) {
			if (uItem==U(TVI_ROOT))
				return pData->uFirstChild;
			return 0;
		}

		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;

		return pEntry->uFirstChild;

	case TVGN_FIRSTVISIBLE:
		if (pData->uItemPosCount<=0)
			return 0;
		if (pData->uItemPosCount<=pData->uScrollY)
			return 0;

		return pData->pItemPos[pData->uScrollY];

	case TVGN_NEXTVISIBLE:
		if (uItem>pData->uTreeItemsMax) {
			if (uItem==U(TVI_ROOT))
				return pData->uFirstChild;
			return 0;
		}

		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;

		uPos=pEntry->uShowPos;
		if (uPos< pData->uScrollY)
			return 0;
		if (uPos>=pData->uScrollY+pData->uPageEnties-1)
			return 0;

		return pData->pItemPos[uPos+1];

	case TVGN_NEXTSELECTED:
		if (uItem>pData->uTreeItemsMax) {
			if (uItem!=U(TVI_ROOT))
				return 0;

			uItem = pData->uFirstChild;

			pEntry = pData->pTreeItems[uItem];
			if (!pEntry)
				return 0;
			if ( pEntry->uState&TVIS_SELECTED)
				return uItem;
		}

		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;

		for (;;) {
			if (pEntry->uFirstChild) {
				uItem = pEntry->uFirstChild;
			} else if (pEntry->uNextItem) {
				uItem = pEntry->uNextItem;
			} else {
				for (;;) {
					uItem  = pEntry->uParent;
					pEntry = pData ->pTreeItems[uItem];
					if (!pEntry)
						return 0;

					if (pEntry->uNextItem) {
						uItem=pEntry->uNextItem;
						break;
					}
				}
			}

			pEntry = pData ->pTreeItems[uItem];
			if (!pEntry)
				break;
									
			if (pEntry->uState & TVIS_SELECTED)
				return uItem;
		}

		return 0;

	case TVGN_NEXTSELCHILD:
		if (uItem > pData->uTreeItemsMax) {
			if (uItem!=U(TVI_ROOT))
				return 0;

			uItem = pData->uFirstChild;

			pEntry = pData->pTreeItems[uItem];
			if (!pEntry)
				return 0;
			if (pEntry->uState & TVIS_SELECTED)
				return uItem;

			uStop = 0;
		} else {
			pEntry = pData->pTreeItems[uItem];
			if (!pEntry || !pEntry->uFirstChild)
				return 0;

			uStop = uItem;
		}

		for (;;) {
			if (pEntry->uFirstChild) {
				uItem = pEntry->uFirstChild;
			} else if (pEntry->uNextItem) {
				uItem = pEntry->uNextItem;
			} else {
				for (;;) {
					uItem= pEntry->uParent;
					if (uItem==uStop)
						return 0;
					pEntry = pData ->pTreeItems[uItem];
					if (!pEntry)
						return 0;
					if (pEntry->uNextItem) {
						uItem=pEntry->uNextItem;
						break;
					}
				}
			}

			pEntry = pData ->pTreeItems[uItem];
			if (!pEntry)
				break;
			if (pEntry->uState & TVIS_SELECTED)
				return uItem;
		}

		return 0;

	case TVGN_PREVIOUSVISIBLE:
		if (uItem>pData->uTreeItemsMax) {
			if (uItem==U(TVI_ROOT))
				return pData->uFirstChild;
			return 0;
		}

		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;

		uPos=pEntry->uShowPos;
		if (uPos<=pData->uScrollY)
			return 0;
		if (uPos>=pData->uScrollY+pData->uPageEnties)
			return 0;

		return pData->pItemPos[uPos-1];

	case TVGN_LASTVISIBLE:
		uPos = pData->uItemPosCount;
		if (uPos<=0)
			return 0;
		return pData->pItemPos[uPos-1];

	case TVGN_DROPHILITE:
		return pData->uTrackedItem;

	case TVGN_DROPHILITESUB:
		return pData->uTrackedSub;

	case TVGN_CARET:
		return pData->uSelectedItem;

	case TVGN_CARETSUB:
		return pData->uSelectedSub;

	case TVGN_FOCUS:
		return (pData->uFocusItem)? pData->uFocusItem:pData->uSelectedItem;

	case TVGN_FOCUSSUB:
		return (pData->uFocusItem)? pData->uFocusSub:pData->uSelectedSub;
	}

	return 0;
}


static unsigned TreeListNextSelUntil(TreeListData *pData,size_t uItem,unsigned uStop)
{
	BaseItem	   *pEntry;

	if (uItem > pData->uTreeItemsMax) {
		if (uItem != U(TVI_ROOT))
			return 0;

		uItem = pData->uFirstChild;

		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;
		if (pEntry->uState & TVIS_SELECTED)
			return uItem;

		uStop = 0;
	} else {
		pEntry = pData->pTreeItems[uItem];
		if (!pEntry)
			return 0;
		if (!pEntry->uFirstChild && uItem==uStop)
			return 0;
	}

	for(;;) {
		if (pEntry->uFirstChild) {
			uItem = pEntry->uFirstChild;
		} else if (pEntry->uNextItem) {
			uItem = pEntry->uNextItem;
		} else {
			for(;;) {
				uItem= pEntry->uParent;
				if (uItem==uStop)
					return 0;

				pEntry = pData ->pTreeItems[uItem];
				if (!pEntry)
					return 0;

				if (pEntry->uNextItem) {
					uItem=pEntry->uNextItem;
					break;
				}
			}
		}

		pEntry = pData ->pTreeItems[uItem];
		if (!pEntry)
			break;
		if (pEntry->uState & TVIS_SELECTED)
			return uItem;
	}

	return 0;
}

static void TreeListMouseClick(TreeListData *pData,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	ExtraItem      *pExtra;
	BaseItem	   *pEntry;
	BaseItem	   *pTemp;
	TV_HITTESTINFO	sInfo;
	NMTREEVIEW		sNotify;
	unsigned		uMaskItem;
	unsigned		uMaskSub;
	size_t			uItem;
	unsigned		uTemp;
	unsigned		uSub;
	int				iMode;


	if (!pData->cIsEnabled) {
		return;
	}

	if (!pData->cHasFocus) {
		UNLOCK(pData);
		SetFocus(pData->hWnd);
		LOCK(pData);
	}

	sInfo.flags = (UINT) wParam;
	sInfo.pt.x	= LOWORD(lParam);
	sInfo.pt.y	= HIWORD(lParam);
	uItem		= TreeListHitTest(pData,&sInfo);

	if (uItem) {
		pEntry = pData->pTreeItems[uItem];

		if (pData->uStyle & TVS_FULLROWSELECT) {
			uMaskItem = TVHT_ONITEMICON|TVHT_ONITEMLABEL|TVHT_ONITEMSTATEICON|TVHT_ONITEMRIGHT;
			uMaskSub  =	TVHT_ONSUBICON |TVHT_ONSUBLABEL |TVHT_ONSUBRIGHT;
		} else {
			uMaskItem = TVHT_ONITEMICON|TVHT_ONITEMLABEL|TVHT_ONITEMSTATEICON;
			uMaskSub  =	TVHT_ONSUBICON |TVHT_ONSUBLABEL;
		}

		if (sInfo.flags & TVHT_ONITEMBUTTON) {
			if (uMsg==WM_LBUTTONDOWN || uMsg==WM_LBUTTONDBLCLK) {
				pEntry = pData->pTreeItems[uItem];
				if (pEntry && (pEntry->uState&(TVIS_EXPANDED|TVIS_EXPANDPARTIAL))==(TVIS_EXPANDED|TVIS_EXPANDPARTIAL)) {
					pEntry->uState &= ~TVIS_EXPANDPARTIAL;
					UpdateRect(pData,uItem,0);
				} else {
					TreeListToggleItem(pData,uItem,0);
				}
			}
		} else if (sInfo.flags&TVHT_ONITEMSTATEICON) {
			if (pData->uStyle&TVS_CHECKBOXES)
				if (uMsg==WM_LBUTTONDOWN || uMsg==WM_LBUTTONDBLCLK) {
					TV_ITEMW sItem;

					sItem.hItem		= (HTREEITEM)uItem;
					sItem.mask		= TVIF_STATE;
					sItem.stateMask	= TVIS_STATEIMAGEMASK;
					sItem.state		= (pEntry->uState&0x1000)?	0x2000:0x1000;

					TreeListSetItem(pData,&sItem);
				}
		} else if (sInfo.flags&uMaskItem) {
			if (!(pData->uStyle&TVS_DISABLEDRAGDROP)) {
				if (uMsg==WM_LBUTTONDOWN) {
					pData->uDragFlags = MK_LBUTTON;
					pData->uDragItem  = uItem;
					pData->uDragSub   = 0;
				}

				if (uMsg==WM_RBUTTONDOWN) {
					pData->uDragFlags = MK_RBUTTON;
					pData->uDragItem  = uItem;
					pData->uDragSub   = 0;
				}
			}

			if (wParam&MK_CONTROL) {
				if (pData->uStyleEx&TVS_EX_MULTISELECT)	{
					pEntry = pData->pTreeItems[uItem];
					if (pEntry && (pEntry->uState&TVIS_SELECTED)) {
						if (uMsg!=WM_LBUTTONDOWN)
							if (uMsg!=WM_LBUTTONDBLCLK)
								goto End;

						TreeListSelectItem(pData,0,0,TVC_BYMOUSE);

						if (TreeListXorSelectItem(pData,uItem,TVC_BYMOUSE)) {								
							pTemp =   pData->pTreeItems[pEntry->uFirstChild];
							if (pTemp && !pTemp->uShowPos)
								for(uTemp=uItem;;)
								{
									uTemp=TreeListNextSelUntil(pData,uTemp,uItem);
									if (!uTemp)
										break;
									TreeListXorSelectItem(pData,uTemp,TVC_BYMOUSE);
								}
						}

						uTemp = pData->uFocusItem;
						if (uTemp) {
							TreeListRemoveFocus(pData);
						}

						uSub = TVHT_SUBTOCOL(sInfo.flags);
						if (!(pData->uStyleEx & TVS_EX_SUBSELECT))
							uSub=0;

						pData->uFocusItem = uItem;
						pData->uFocusSub  = uSub;

						if (uSub) {
							pExtra =pData->pExtraItems[uSub-1][uItem];
							if(pExtra)pExtra->bFlags |= TVIX_FOCUSED;
						} else {
							pEntry->bFlags |= TVIX_FOCUSED;
						}

						UpdateRect(pData,uItem,uSub);

						goto End;
					}
				}

			iMode = TVC_BYMOUSE;
		} else {
			iMode = TVC_BYMOUSE|TVC_DESELECT;
		}

		TreeListRemoveFocus  (pData);
		TreeListSelectItem   (pData,uItem,0,iMode);
		TreeListEnsureVisible(pData,pData->uSelectedItem,pData->uSelectedSub);
		pEntry = pData->pTreeItems[uItem];

		if (uMsg==WM_LBUTTONDBLCLK) {
			if (pEntry && (pEntry->uState&(TVIS_EXPANDED|TVIS_EXPANDPARTIAL))==(TVIS_EXPANDED|TVIS_EXPANDPARTIAL)) {
				pEntry->uState &= ~TVIS_EXPANDPARTIAL;
				UpdateRect(pData,uItem,0);
			} else {
				TreeListToggleItem(pData,uItem,0);
			}
		}
	} else if (sInfo.flags & uMaskSub) {
		if (pData->uStyleEx&TVS_EX_SUBSELECT)
			uSub = TVHT_SUBTOCOL(sInfo.flags);
		else
			uSub = 0;

		if (!(pData->uStyle&TVS_DISABLEDRAGDROP)) {
			if (uMsg==WM_LBUTTONDOWN) {
				pData->uDragFlags = MK_LBUTTON;
				pData->uDragItem  = uItem;
				pData->uDragSub   = uSub;
			}

			if (uMsg==WM_RBUTTONDOWN) {
				pData->uDragFlags = MK_RBUTTON;
				pData->uDragItem  = uItem;
				pData->uDragSub   = uSub;
			}
		}

		if (wParam & MK_CONTROL) {
			if (pData->uStyleEx&TVS_EX_MULTISELECT)	{
				pEntry = pData->pTreeItems[uItem];
				if (pEntry && (pEntry->uState&TVIS_SELECTED)) {
					if (uMsg!=WM_LBUTTONDOWN)
						if (uMsg!=WM_LBUTTONDBLCLK)
							goto End;

					TreeListSelectItem(pData,0,0,TVC_BYMOUSE);

					if (TreeListXorSelectItem(pData,uItem,TVC_BYMOUSE)) {								
						pTemp =   pData->pTreeItems[pEntry->uFirstChild];
						if ( pTemp && !pTemp->uShowPos)
							for (uTemp=uItem;;) {
								uTemp=TreeListNextSelUntil(pData,uTemp,uItem);
								if (!uTemp)
									break;
								TreeListXorSelectItem(pData,uTemp,TVC_BYMOUSE);
							}
					}

					TreeListRemoveFocus(pData);

					pData->uFocusItem = uItem;
					pData->uFocusSub  = uSub;

					if (uSub) {
						pExtra =pData->pExtraItems[uSub-1][uItem];
						if (pExtra)
							pExtra->bFlags |= TVIX_FOCUSED;
					} else {
						pEntry->bFlags |= TVIX_FOCUSED;
					}

					UpdateRect(pData,uItem,uSub);

					goto End;
				}
			}

			iMode = TVC_BYMOUSE;
		} else {
			iMode = TVC_BYMOUSE|TVC_DESELECT;
		}

		TreeListRemoveFocus  (pData);
		TreeListSelectItem   (pData,uItem,uSub,(wParam&MK_CONTROL)? TVC_BYMOUSE:TVC_BYMOUSE|TVC_DESELECT);
		TreeListEnsureVisible(pData,pData->uSelectedItem,pData->uSelectedSub);
		pEntry = pData->pTreeItems[uItem];

		if (uMsg==WM_LBUTTONDBLCLK) {
			TreeListToggleItem(pData,uItem,0);
		}
	}
	} else {
		pEntry = NULL;
		uSub   = 0;
	}

End:
	switch(uMsg) {
	case WM_LBUTTONDOWN:	sNotify.hdr.code = NM_CLICK;	break;
	case WM_LBUTTONDBLCLK:	sNotify.hdr.code = NM_DBLCLK;	break;
	case WM_RBUTTONDOWN:	sNotify.hdr.code = NM_RCLICK;	break;
	case WM_RBUTTONDBLCLK:	sNotify.hdr.code = NM_RDBLCLK;	break;
	}

	if (pEntry) {
		sNotify.itemNew.stateMask	= 0xFFFFFFFF;
		sNotify.itemNew.state		= pEntry->uState;
		sNotify.itemNew.lParam		= pEntry->lParam;
		sNotify.itemNew.cChildren	= TVHT_SUBTOCOL(sInfo.flags);
	} else {
		sNotify.itemNew.stateMask	= 0;
		sNotify.itemNew.state		= 0;
		sNotify.itemNew.lParam		= 0;
	}

	sNotify.action				= 0;
	sNotify.itemNew.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBNUMBER;
	sNotify.itemNew.hItem		= (HTREEITEM)uItem;
	sNotify.itemNew.pszText		= (LPTSTR)-1;
	sNotify.itemNew.cchTextMax	= -1;
	sNotify.itemOld.mask		= 0;
	sNotify.ptDrag.x			= LOWORD(lParam);
	sNotify.ptDrag.y			= HIWORD(lParam);

	UNLOCK(pData);

	SendNotify(pData,&sNotify.hdr);
}

static void TreeListKeyDown(TreeListData *pData,WPARAM wParam,LPARAM lParam)
{
	BaseItem	   *pTemp;
	BaseItem	   *pEntry;
	TV_KEYDOWN		sNotify;
	unsigned		uTemp;
	unsigned		uItem;
	unsigned		uVal;
	int				iOldLine;
	int				iOldSub;
	int				iBase;
	int				iLine;
	int				iAdd;
	int				iPos;
	int				iSub;
	int				iMax;
	int				iDel;

	if (!pData->cIsEnabled)	{
		return;
	}

	if (!pData->cHasFocus) {
		UNLOCK(pData);
		SetFocus(pData->hWnd);
		LOCK(pData);
	}

	if (wParam==VK_RETURN) {
		UNLOCK(pData);

		sNotify.hdr.code	=  NM_RETURN;
		sNotify.wVKey		= (WORD)wParam;
		sNotify.flags		= (UINT)lParam;

		SendNotify(pData,&sNotify.hdr);
		
		return;
	}

	pEntry = pData->pTreeItems[pData->uSelectedItem];
	iDel   = (GetAsyncKeyState(VK_SHIFT)&0x8000) ? 0 : TVC_DESELECT;

	if (GetAsyncKeyState(VK_CONTROL) & 0x8000) {
		iLine = pData->uScrollY;
		iSub  = pData->uScrollX;

		switch(wParam) {
		case VK_END:
			iLine  = pData->uTreeItemsCount;	
			iLine -= pData->uPageEnties;
			break;

		case VK_HOME:
			iLine=0;							
			break;

		case VK_LEFT:
			iSub-=16;							
			break;

		case VK_RIGHT:
			iSub+=16;							
			break;

		case VK_UP:
			iLine--;							
			break;

		case VK_DOWN:
			iLine++;							
			break;

		case VK_PRIOR:
			iLine-=pData->uSizeX;				
			break;

		case VK_NEXT:
			iLine+=pData->uSizeX;				
			break;
															
		case VK_SPACE:
			if (pEntry==NULL)
				break;
			if (pEntry->bFlags&TVIX_HASBUTTON) {
				TreeListToggleItem(pData,pData->uSelectedItem,0);
			}			
		}

		if (iLine!=(int)pData->uScrollY) {
			iMax  = pData->uItemPosCount;
			iMax -=	pData->uPageEnties-1;

			if (iLine>=iMax)
				iLine=iMax;
			if (iLine<0)
				iLine=0;
			if (iLine!=(int)pData->uScrollY) {
				pData->uScrollY = iLine;
				SetScrollPos(pData->hWnd,SB_VERT,iLine,TRUE);
				UpdateView(pData);
			}
		}

		if (iSub!=(int)pData->uScrollX) {
			uVal = pData->uColumnCount;
			if (uVal)
				iMax  = pData->iColumnXpos[uVal]-pData->uSizeX/2;
			else
				iMax  = pData->iMaxSizeX;
			iMax -=	pData->uSizeX/2;

			if (iSub>=iMax)
				iSub=iMax;
			if (iSub<0)
				iSub=0;
			if (iSub!=(int)pData->uScrollX) {
				pData->uScrollX = iSub;
				SetScrollPos(pData->hWnd,SB_HORZ,iSub,TRUE);
				UpdateView(pData);

				if (pData->hHeader) {
					MoveWindow(pData->hHeader,-iSub,0,pData->uSizeX-1+iSub,pData->uStartPixel,TRUE);
				}
			}
		}
	} else {													
		iSub  = pData->uSelectedSub;

		if (!pEntry) {
			iLine    =  0;
			iSub     =  0;
			iOldSub	 =  0;
			iOldLine = -1;

			uTemp = pData->uFocusItem;
			if (uTemp) {
				pTemp = pData->pTreeItems[uTemp];

				while (pTemp && !pTemp->uShowPos) {
					pTemp = pData->pTreeItems[pTemp->uParent];
				}

				if (pTemp) {
					iLine = pTemp->uShowPos-1;
					iSub  = pData->uFocusSub;
				}

				TreeListRemoveFocus(pData);
			}
		} else {
			iLine =  pEntry->uShowPos-1;
			if (iLine<0)
				iLine=0;

			iOldLine =  iLine;
			iOldSub	 =  iSub;
		}

			switch (wParam) {
			case VK_END:	iLine=pData->uItemPosCount-1;	break;
			case VK_HOME:	iLine=0;						break;
			case VK_LEFT:
				if (pData->uColumnCount>1 &&
					(pData->uStyleEx&TVS_EX_SUBSELECT)) {
					iSub--;
				} else {
					if (! pEntry)
						break;
					if (!(pEntry->uState&TVIS_EXPANDED) || 
						!pEntry->uFirstChild) {
						pTemp = pData->pTreeItems[pEntry->uParent];
						if (pTemp)
							iLine=pTemp->uShowPos-1;
						break;
					}

					if (pEntry->bFlags&TVIX_HASBUTTON) {
						TreeListToggleItem(pData,pData->uSelectedItem,0);
					}
				}
				break;

			case VK_RIGHT:
				if (pData->uColumnCount>1 &&
					(pData->uStyleEx&TVS_EX_SUBSELECT)) {
					iSub++;
				} else {
					if (!pEntry)
						break;
					if (pEntry->uState&TVIS_EXPANDED) {
						iLine++;
						break;
					}

					if (pEntry->bFlags&TVIX_HASBUTTON) {
						TreeListToggleItem(pData,pData->uSelectedItem,0);
					}
				}

				break;

			case VK_UP:		iLine--;						break;
			case VK_DOWN:	iLine++;						break;

			case VK_PRIOR:	
				iAdd=pData->uPageEnties-1;			
				if (iAdd<=0)
					iAdd=1;
					iLine-=iAdd;
				break;

			case VK_NEXT:
				iAdd=pData->uPageEnties-1;			
				if (iAdd<=0)
					iAdd=1;
				iLine+=iAdd;
				break;

			case VK_BACK:		  								
				if (pEntry) {
					uItem = pEntry->uParent;
					if (!uItem)
						iLine=0;
					else {
						iLine = pData->pTreeItems[uItem]->uShowPos-1;
						if (iLine<0)
							iLine=0;
					}
				}
				break;

			case VK_SPACE:
				if (pEntry && iSub==0 && (pData->uStyle&TVS_CHECKBOXES)) {
					TVITEMW sItem;

					sItem.hItem		= (HTREEITEM)pData->uSelectedItem;
					sItem.mask		= TVIF_STATE;
					sItem.stateMask	= TVIS_STATEIMAGEMASK;
					sItem.state		= (pEntry->uState&0x1000)?	0x2000:0x1000;

					TreeListSetItem(pData,&sItem);
				}
			}

			if (iSub>=(int)pData->uColumnCount)
				iSub=pData->uColumnCount-1;
			if (iSub<0)
				iSub=0;
			if (iLine>=(int)pData->uItemPosCount)
				iLine=pData->uItemPosCount-1;
			if (iLine<0)
				iLine=0;

			if (!(pData->uStyleEx&TVS_EX_SUBSELECT))
				iSub=0;

			if (pData->uItemPosCount>0)
				if (iLine!=iOldLine || iSub!=iOldSub) {
					uItem = pData->pItemPos[iLine];
					TreeListSelectItem(pData,uItem,iSub,TVC_BYKEYBOARD|iDel);
					TreeListEnsureVisible(pData,pData->uSelectedItem,pData->uSelectedSub);

					if ((pData->uStyleEx&TVS_EX_MULTISELECT) && !iDel) {
						uVal  = pData->uSelectedBase;
						pTemp = pData->pTreeItems[uVal];
						iBase =(pTemp)? pTemp->uShowPos-1:-1;

						if (iLine>iOldLine) {
							iMax = (iOldLine>iBase)? iOldLine+1:iOldLine;
							if (iMax<0)
								iMax=0;
							iPos = iLine-1;
						} else {
							iMax = iLine+1;
							iPos = (iOldLine<iBase)? iOldLine-1:iOldLine;
							if (iPos<0)
								iPos=0;
						}

						for (;iPos>=iMax;iPos--) {
							if (iPos==iBase)
								continue;
							uItem = pData->pItemPos[iPos];
							if (!TreeListXorSelectItem(pData,uItem,TVC_BYKEYBOARD))
								continue;

							pTemp = pData->pTreeItems[uItem];	
							if (pTemp && !(pTemp->uState&TVIS_SELECTED)) {
								pTemp =  pData->pTreeItems[pTemp->uFirstChild];
								if (!pTemp || pTemp->uShowPos)
									continue;

							for (uTemp=uItem;;) {
								uTemp=TreeListNextSelUntil(pData,uTemp,uItem);
								if (!uTemp)
									break;
								TreeListXorSelectItem(pData,uTemp,TVC_BYKEYBOARD);
							}
						}
					}
				}
			}
	}

	sNotify.hdr.code	=  TVN_KEYDOWN;
	sNotify.wVKey		= (WORD)wParam;
	sNotify.flags		= (UINT)lParam;

	UNLOCK(pData);

	SendNotify(pData,&sNotify.hdr);
}


static int TreeListSortItemsCb(TreeListData *pData,TV_SORTCB *pSortData,int iMode)
{
	TV_SORTCB	sSort;
	size_t		uParent;
	size_t		uFirst;
	size_t		uItem;
	BaseItem   *pEntry;
	BaseItem   *pNext;
	BaseItem  **pList;

	uParent=U(pSortData->hParent);
	if (uParent>pData->uTreeItemsMax) {
		if (uParent!=U(TVI_ROOT))
			return 0;
		
		uFirst = pData->uFirstChild;
		if (uFirst==0)
			return 1;
		if (uFirst==pData->uLastChild) {
			if (uFirst) {
				sSort.hParent		= (HTREEITEM)uFirst;
				sSort.lParam		= pSortData->lParam;
				sSort.lpfnCompare	= pSortData->lpfnCompare;
				TreeListSortItemsCb(pData,&sSort,SORT_NOUPDATE);
			}
			return 1;
		}

		pList   = pData->pTreeItems;
		uParent = 0;
	} else {
		pList = pData->pTreeItems;

		pEntry = pList[uParent];
		if (pEntry==NULL)
			return 0;

		uFirst = pEntry->uFirstChild;
		if (uFirst==0)
			return 1;
		if (uFirst==pEntry->uLastChild) {
			if (uFirst) {
				sSort.hParent		= (HTREEITEM)uFirst;
				sSort.lParam		= pSortData->lParam;
				sSort.lpfnCompare	= pSortData->lpfnCompare;
				TreeListSortItemsCb(pData,&sSort,SORT_NOUPDATE);
			}
			return 1;
		}
	}


	if (iMode) {
		pNext				= pList[uFirst];
		sSort.hParent		= (HTREEITEM)uFirst;
		sSort.lParam		= pSortData->lParam;
		sSort.lpfnCompare	= pSortData->lpfnCompare;

		do	{
			TreeListSortItemsCb(pData,&sSort,SORT_NOUPDATE);
			sSort.hParent	= (HTREEITEM)pNext->uNextItem;
			pNext			= pList[pNext->uNextItem];
		}while(pNext);
	}

	unsigned   *pItemList;
	unsigned   *pItemNew;
	unsigned	uEnties[128];
	unsigned	uPos;
	unsigned	uMax;

	uItem		= uFirst;
	pItemList	= uEnties;
	uMax		= 128;
	uPos		= 0;

	do	{													
		if(uPos>=uMax) {
			uMax *= 2;
			pItemNew	= malloc(sizeof(unsigned)*uMax);
			memcpy(pItemNew,pItemList,uPos*sizeof(pItemList[0]));
			if(uPos>128) free(pItemList);
			pItemList = pItemNew;
		}

		pItemList[uPos] = uItem;
		pNext			= pList[uItem];
		uItem			= pNext->uNextItem;
		uPos++;
	} while(uItem);


#define XCHANGE_MEM(a,b)	uTemp=pItemList[a];pItemList[a]=pItemList[b];pItemList[b]=uTemp;
	PFNTVCOMPARE	pCompare;
	LPARAM			lParamTemp;
	LPARAM			lParamSort;
	int				iLower,iUpper,iMiddle,iCmp;
	int				uMemL[30],uMemU[30];
	int				iStart,iLast;
	int				iLevel;
	unsigned		uTemp;

	pData->cLockChanges=1;
	UNLOCK(pData);

	pCompare	= pSortData->lpfnCompare;
	lParamSort	= pSortData->lParam;
	iLast		= uPos-1;
	iStart		= 0;
	iLevel		= 0;

	for (;;) {
		iLower	=  iStart;
		iMiddle = (iStart+iLast)>>1;						
		iUpper	=  iLast +1;

		XCHANGE_MEM(iMiddle,iLower);

		uItem	   = pItemList[iStart];
		lParamTemp = pList[uItem]->lParam;

		for (;;) {
			do  {
				iLower++;
				if(iLower>iLast)break;
				uItem = pItemList[iLower];
				iCmp=pCompare(pList[uItem]->lParam,lParamTemp,lParamSort);
			} while (iCmp<=0);

			do {
				iUpper--;
				if(iUpper<=iStart)
					break;
				uItem = pItemList[iUpper];
				iCmp=pCompare(pList[uItem]->lParam,lParamTemp,lParamSort);
			} while (iCmp>=0);

			if (iUpper<iLower)
				break;

			XCHANGE_MEM(iUpper,iLower);
		}

		XCHANGE_MEM(iStart,iUpper);

		if (iUpper-1-iStart >= iLast-iLower) {
			if (iStart+1 < iUpper) {
				uMemL[iLevel] = iStart;
				uMemU[iLevel] = iUpper-1;
				iLevel++;
			}
			if (iLower < iLast) {
				iStart = iLower;
				continue;
			}
		} else {
			if (iLower<iLast) {
				uMemL[iLevel] = iLower;
				uMemU[iLevel] = iLast;
				iLevel++;
			}

			if (iStart+1 < iUpper) {
				iLast = iUpper-1;
				continue;
			}
		}
															
		iLevel--;

		if (iLevel>=0) {
			iStart = uMemL[iLevel];
			iLast  = uMemU[iLevel];
			continue;
		}

		break;
	}

	LOCK(pData);
	pData->cLockChanges=0;

	unsigned	uNum;
	unsigned	uLast;

	uPos--;

	pEntry = pList[uParent];
	if (!pEntry) {
		pData->uFirstChild  = pItemList[ 0  ];
		pData->uLastChild   = pItemList[uPos];
	} else {
		pEntry->uFirstChild = pItemList[ 0  ];
		pEntry->uLastChild  = pItemList[uPos];
	}
	
	uLast  = 0;
	uItem  = pItemList[0];

	for (uNum=0;uNum<uPos;) {
		pEntry = pList[uItem];
		pEntry->uPrevItem = uLast;

		uNum++;
		uLast  = uItem;
		uItem  = pItemList[uNum];

		pEntry->uNextItem = uItem;
	}

	pEntry = pList[uItem];
	pEntry->uPrevItem = uLast;
	pEntry->uNextItem = 0;

	if (iMode!=SORT_NOUPDATE) {
		UpdateItems(pData,uParent);

		if (pData->uStyle & TVS_SHOWSELALWAYS)
			if (pData->uSelectedItem) {
				TreeListEnsureVisible(pData,pData->uSelectedItem,pData->uSelectedSub);
			}
	}

	if (uMax>128)
		free(pItemList);

	return 1;
}


static int TreeListSortItems(TreeListData *pData,unsigned uParent,int iMode)
{
	unsigned	uItem;
	unsigned	uFirst;
	BaseItem   *pEntry;
	BaseItem   *pNext;
	BaseItem  **pList;

	if (uParent>pData->uTreeItemsMax) {
		if (uParent!=U(TVI_ROOT))
			return 0;
		
		uFirst = pData->uFirstChild;
		if (uFirst==0)
			return 1;
		if (uFirst==pData->uLastChild) {
			if (uFirst) {
				TreeListSortItems(pData,uFirst,SORT_NOUPDATE);
			}
			return 1;
		}
		
		pList   = pData->pTreeItems;
		uParent = 0;
	} else {
		pList = pData->pTreeItems;

		pEntry = pList[uParent];
		if (pEntry==NULL)
			return 0;

		uFirst = pEntry->uFirstChild;
		if (uFirst==0)
			return 1;
		if (uFirst==pEntry->uLastChild) {
			if (uFirst) {
				TreeListSortItems(pData,uFirst,SORT_NOUPDATE);
			}
			return 1;
		}
	}

	if (iMode) {
		uItem = uFirst;

		do	{
			TreeListSortItems(pData,uItem,SORT_NOUPDATE);
			pNext = pList[uItem];
			uItem = pNext->uNextItem;
		} while(uItem);
	}

	unsigned   *pItemList;
	unsigned   *pItemNew;
	unsigned	uEnties[128];
	unsigned	uPos;
	unsigned	uMax;

	uItem		= uFirst;
	pItemList	= uEnties;
	uMax		= 128;
	uPos		= 0;

	do	{													
		if (uPos>=uMax) {
			uMax *= 2;
			pItemNew	= malloc(sizeof(unsigned)*uMax);
			memcpy(pItemNew,pItemList,uPos*sizeof(pItemList[0]));
			if(uPos>128) free(pItemList);
			pItemList = pItemNew;
		}

		pItemList[uPos] = uItem;
		pNext			= pList[uItem];
		uItem			= pNext->uNextItem;
		uPos++;
	} while(uItem);


#define XCHANGE_MEM(a,b)	uTemp=pItemList[a];pItemList[a]=pItemList[b];pItemList[b]=uTemp;

	LPCWSTR			pTextTemp;
	LPCWSTR			pText;
	int				iLower,iUpper,iMiddle,iCmp;
	int				uMemL[30],uMemU[30];
	int				iStart,iLast;
	int				iLevel;
	int				iNone;
	unsigned		uSize;
	unsigned		uTemp;


	pData->cLockChanges=1;
	UNLOCK(pData);

	iLast		= uPos-1;
	iStart		= 0;
	iLevel		= 0;

	for (;;) {
		iLower	=  iStart;
		iMiddle = (iStart+iLast)>>1;						
		iUpper	=  iLast +1;

		XCHANGE_MEM(iMiddle,iLower);

		uItem  = pItemList[iStart];
		pEntry = pList[uItem];
		if (pEntry->bCallback&TVIF_TEXT) {
			uSize=0;
			LOCK(pData);
			CallbackEntry(pData,pEntry,uItem,TVIF_TEXT,&iNone,&uSize,&pTextTemp);
			UNLOCK(pData);
		} else {
			pTextTemp = pEntry->pText;
		}

		for (;;) {
			do  {
				iLower++;
				if (iLower>iLast) break;

				uItem  = pItemList[iLower];
				pEntry = pList[uItem];
				if (pEntry->bCallback & TVIF_TEXT) {
					uSize=1;
					LOCK(pData);
					CallbackEntry(pData,pEntry,uItem,TVIF_TEXT,&iNone,&uSize,&pText);
					UNLOCK(pData);
				} else {
					pText = pEntry->pText;
				}

				iCmp=pscmp((PortString) pText,(PortString) pTextTemp);
			} while (iCmp<=0);

			do {
				iUpper--;

				if (iUpper<=iStart)
					break;

				uItem  = pItemList[iUpper];
				pEntry = pList[uItem];

				if (pEntry->bCallback&TVIF_TEXT) {
					LOCK(pData);
					CallbackEntry(pData,pEntry,uItem,TVIF_TEXT,&iNone,&uSize,(LPCWSTR*) &pText);
					UNLOCK(pData);
				} else {
					pText = pEntry->pText;
				}

				iCmp=pscmp((PortString) pText,(PortString) pTextTemp);
			} while(iCmp>=0);

			if (iUpper<iLower)
				break;

			XCHANGE_MEM(iUpper,iLower);
		}

		XCHANGE_MEM(iStart,iUpper);

		if (iUpper-1-iStart >= iLast-iLower) {
			if (iStart+1 < iUpper) {
				uMemL[iLevel] = iStart;
				uMemU[iLevel] = iUpper-1;
				iLevel++;
			}
			if(iLower < iLast) {
				iStart = iLower;
				continue;
			}
		} else {
			if(iLower<iLast) {
				uMemL[iLevel] = iLower;
				uMemU[iLevel] = iLast;
				iLevel++;
			}

			if(iStart+1 < iUpper) {
				iLast = iUpper-1;
				continue;
			}
		}
															
		iLevel--;

		if (iLevel>=0) {
			iStart = uMemL[iLevel];
			iLast  = uMemU[iLevel];
			continue;
		}
		break;
	}

	LOCK(pData);
	pData->cLockChanges=0;

	unsigned	uNum;
	unsigned	uLast;

	uPos--;

	pEntry = pList[uParent];
	if(!pEntry) {
		pData->uFirstChild  = pItemList[ 0  ];
		pData->uLastChild   = pItemList[uPos];
	} else {
		pEntry->uFirstChild = pItemList[ 0  ];
		pEntry->uLastChild  = pItemList[uPos];
	}

	uLast  = 0;
	uItem  = pItemList[0];

	for (uNum=0;uNum<uPos;) {
		pEntry = pList[uItem];
		pEntry->uPrevItem = uLast;

		uNum++;
		uLast  = uItem;
		uItem  = pItemList[uNum];

		pEntry->uNextItem = uItem;
	}

	pEntry = pList[uItem];
	pEntry->uPrevItem = uLast;
	pEntry->uNextItem = 0;

	if (iMode!=SORT_NOUPDATE) {
		UpdateItems(pData,uParent);

		if (pData->uStyle & TVS_SHOWSELALWAYS)
			if (pData->uSelectedItem) {
				TreeListEnsureVisible(pData,pData->uSelectedItem,pData->uSelectedSub);
			}
	}

	if (uMax>128) free(pItemList);

	return 1;
}


static int TreeListEndLabelEdit(TreeListData *pData,int iCancel)
{
	WCHAR			cText[260];
	NMTVDISPINFOW	sNotify;
	TV_ITEMW		sSet;
	LRESULT			lRet;
	unsigned		uSub;
	size_t			uItem;
	ExtraItem	   *pExtra;
	BaseItem	   *pEntry;
	char			cCb;

	uItem = pData->uEditItem;
	uSub  = pData->uEditSub;
	cCb	  =	pData->uEditCb;

	pData->uEditItem = 0;
	pData->uEditSub  = 0;
	pData->uEditCb	 = 0;

	if (uItem>pData->uTreeItemsMax || uSub>=pData->uColumnCount) {
		return 0;
	}

   	pEntry = pData->pTreeItems[uItem];
	if (!pEntry) {
		return 0;
	}

	if (iCancel) {
		sNotify.item.cchTextMax = 0;
		sNotify.item.pszText	= NULL;
	} else {
		GetWindowTextW(pData->hEdit,cText,sizeof(cText)/sizeof(cText[0]));
		cText[sizeof(cText)/sizeof(cText[0])-1]=0;
		sNotify.item.cchTextMax = sizeof(cText)/sizeof(cText[0])-1;
		sNotify.item.pszText	= cText;
	}


	sNotify.hdr.code			= TVN_ENDLABELEDIT;
	sNotify.item.mask			= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_TEXT|TVIF_SUBITEM;
	sNotify.item.hItem			= (HTREEITEM)uItem;
	sNotify.item.stateMask		= 0xFFFFFFFF;
	sNotify.item.cChildren		= uSub;

	if (uSub) {
		pExtra = pData->pExtraItems[uSub-1][uItem];
		if (!pExtra) {
			sNotify.item.state	= 0;
			sNotify.item.lParam	= 0;
		} else {
			sNotify.item.state  = pEntry->uState&TVIS_BASEFLAGS;
			sNotify.item.state |= pExtra->uState;
			sNotify.item.lParam	= pEntry->lParam;
		}
	} else {
		sNotify.item.state	= pEntry->uState;
		sNotify.item.lParam	= pEntry->lParam;
	}

	UNLOCK(pData);
	ShowWindow(pData->hEdit,SW_HIDE);
	lRet=SendNotify(pData,&sNotify.hdr);
	LOCK(pData);

	if (lRet || iCancel)
		return 0;

	if (cCb) {
		sNotify.hdr.code		= TVN_SETDISPINFO;
		sNotify.item.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_TEXT|TVIF_SUBITEM;
		sNotify.item.stateMask	= (UINT)~TVIS_BASEFLAGS;
		sNotify.item.hItem		= (HTREEITEM)uItem;
		sNotify.item.cchTextMax = sizeof(cText)/sizeof(cText[0])-1;
		sNotify.item.pszText	= cText;
		sNotify.item.cChildren	= uSub;

		UNLOCK(pData);
		SendNotify(pData,&sNotify.hdr);
		LOCK(pData);
	} else {
		sSet.mask		= TVIF_SUBITEM|TVIF_TEXT;
		sSet.cchTextMax	= sizeof(cText)/sizeof(cText[0]);
		sSet.hItem		= (HTREEITEM)uItem;
		sSet.cChildren	= uSub;
		sSet.pszText	= cText;

		TreeListSetItem(pData,&sSet);
	}

	return 1;
}

static HWND TreeListEditLabel(TreeListData *pData,size_t uItem,unsigned uSub)
{
	HDC				hDc;
	HWND			hWnd;
	LRESULT			lRet;
	LPARAM			lParam;
	ExtraItem	   *pExtra;
	BaseItem	   *pEntry;
	NMTVDISPINFOW   sNotify;
	WNDPROC	        pProc;
	LPWSTR			pText;
	RECT			sRect;
	HFONT			hFont;
	unsigned		uCol;
	unsigned		uBits;
	unsigned		uSize;
	unsigned		uState;
	unsigned		uHeight;
	int				iPixels;
	int				iWidth;
	int				iTemp;

	uBits	= uSub>>29;
	uSub   &= 0x1FFFFFFF;

	if (uSub >=pData->uColumnCount)
		return NULL;
	if (uItem> pData->uTreeItemsMax)
		return NULL;
	pEntry=pData->pTreeItems[uItem];
	if (!pEntry)
		return NULL;

	if (uItem!=pData->uSelectedItem || uSub!=pData->uSelectedSub) {
		uCol  = (pData->uStyleEx&TVS_EX_SUBSELECT)? uSub:0;
		iTemp = TreeListSelectItem(pData,uItem,uCol,TVC_UNKNOWN);
	}

	TreeListEnsureVisible(pData,uItem,uSub);

	if (pData->hEdit && (uBits&3)!=pData->uEditMode) {
		DestroyWindow(pData->hEdit);
		pData->hEdit=0;
	}

	if (!pData->hEdit) {
		switch(uBits&3) {
		case 1:
			pData->hEdit=CreateWindowW(L"COMBOBOX",NULL,WS_BORDER|WS_CHILD|ES_AUTOHSCROLL|ES_LEFT|CBS_DROPDOWN,0,0,0,0,pData->hWnd,NULL,ghModule,NULL);
			pData->uEditMode=1;
			break;

		case 2:
			pData->hEdit=CreateWindowW(L"COMBOBOX",NULL,WS_BORDER|WS_CHILD|ES_AUTOHSCROLL|ES_LEFT|CBS_DROPDOWNLIST,0,0,0,0,pData->hWnd,NULL,ghModule,NULL);
			pData->uEditMode=2;
			break;

		default:
			pData->hEdit=CreateWindowW(L"EDIT"    ,NULL,WS_BORDER|WS_CHILD|ES_AUTOHSCROLL|ES_LEFT,0,0,0,0,pData->hWnd,NULL,ghModule,NULL);
			pData->uEditMode=0;
			break;
		}

		if (!pData->hEdit)
			return NULL;

		pProc = (WNDPROC)GetWindowLongPtrW(pData->hEdit,GWLP_WNDPROC);

		SetWindowLongPtrW(pData->hEdit,GWLP_USERDATA,(LPARAM)pProc);
		SetWindowLongPtrW(pData->hEdit,GWLP_WNDPROC,(LPARAM)EditProc);
		SetWindowLongPtrW(pData->hEdit,GWLP_ID,3);

		hWnd = GetWindow(pData->hEdit,GW_CHILD);
				
		while (hWnd) {
			pProc = (WNDPROC)GetWindowLongPtrW(hWnd,GWLP_WNDPROC);
			SetWindowLongPtrW(hWnd,GWLP_USERDATA,(LPARAM)pProc);
			SetWindowLongPtrW(hWnd,GWLP_WNDPROC,(LPARAM)EditProc);
			hWnd = GetNextWindow(hWnd,GW_HWNDNEXT);
		}

		if(!pData->hEdit)
			return NULL;
	}

	pData->uEditCb = 0;

	if (pData->uEditMode>=1) {
		SendMessage(pData->hEdit,CB_RESETCONTENT,0,0);
	}

	if (uSub==0) {
		TreeListGetItemRect(pData,uItem,TVIR_GETCOLUMN|TVIR_TEXT,&sRect);

		pText	= pEntry->pText;
		uSize	= pEntry->uTextSize;
		iPixels	= pEntry->iTextPixels+10;
		lParam	= pEntry->lParam;
		uState	= pEntry->uState;
		hFont	=(uState&TVIS_BOLD)? pData->hFontB:pData->hFontN;

		if (pEntry->bCallback & TVIF_TEXT) {
			CallbackEntry(pData,pEntry,uItem,TVIF_TEXT,&iTemp,&uSize,(LPCWSTR*) &pText);
			hDc=GetDC    (pData->hWnd);
			SelectObject (hDc,hFont);
			DrawTextW    (hDc,pText,0,&sRect,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX|DT_CALCRECT);
			ReleaseDC    (pData->hWnd,hDc);
			iPixels  = sRect.right-sRect.left+10;
			pData->uEditCb=1;
		}

		if (uBits&4) {
			if(pEntry->iImage!=TV_NOIMAGE) {		
				sRect.right  = pData->iColumnXpos[1];
				sRect.right -= pData->uScrollX;
			}

			iPixels	= sRect.right-sRect.left-2;
		} else {
			if(pText)iPixels += pslen(pText);
		}
	} else {													
		if (uBits&4)
			TreeListGetItemRect(pData,uItem,TVIR_GETCOLUMN|          TVIR_COLTOSUB(uSub),&sRect);
		else
			TreeListGetItemRect(pData,uItem,TVIR_GETCOLUMN|TVIR_TEXT|TVIR_COLTOSUB(uSub),&sRect);

		pExtra = pData->pExtraItems[uSub-1][uItem];
		if (!pExtra) {
			pText	= pData->cTempText1;
			uSize	= sizeof(pData->cTempText1)/sizeof(pData->cTempText1[0]);
			iPixels	= sRect.right-sRect.left+10;
			hFont	= pData->hFontN;
			uState  = pEntry->uState&TVIS_BASEFLAGS;
			lParam	= 0;
			pText[0]= 0;
		} else {
			pText	= pExtra->pText;
			uSize	= pExtra->uTextSize;
			iPixels	= pExtra->iTextPixels+10;
			lParam	= pEntry->lParam;
			uState	= pExtra->uState;
			uState |= pEntry->uState&TVIS_BASEFLAGS;
			hFont	=(uState&TVIS_BOLD)? pData->hFontB:pData->hFontN;

			if(pExtra->bCallback&TVIF_TEXT) {
				CallbackExtra(pData,pEntry,pExtra,uItem,uSub,TVIF_TEXT,&iTemp,&uSize,(LPCWSTR*) &pText);
				hDc=GetDC    (pData->hWnd);
				SelectObject (hDc,hFont);
				DrawTextW    (hDc,pText,0,&sRect,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX|DT_CALCRECT);
				ReleaseDC    (pData->hWnd,hDc);
				iPixels=sRect.right-sRect.left;
				pData->uEditCb=1;
			}
		}
		
		if(uBits&4) {
			if(pExtra && pExtra->iImage!=TV_NOIMAGE) {
				sRect.left += pData->iImagesXsize+1;
			}
					
			iPixels	= sRect.right-sRect.left-2;
		} else {
			if (pText) iPixels += pslen(pText);

			switch (pData->cColumnAlign[uSub]) {
			case DT_RIGHT:
				iWidth		= sRect.right-sRect.left;
				sRect.left += iWidth-iPixels;
				break;

			case DT_CENTER:
				iWidth		= sRect.right-sRect.left;
				sRect.left += (iWidth-iPixels)/2;
				break;
			}
		}
	}

	UNLOCK(pData);

	sNotify.hdr.code			= TVN_BEGINLABELEDIT;
	sNotify.item.mask			= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_TEXT|TVIF_SUBITEM;
	sNotify.item.hItem			= (HTREEITEM)uItem;
	sNotify.item.lParam			= lParam;
	sNotify.item.state			= uState;
	sNotify.item.pszText		= pText;
	sNotify.item.cchTextMax		= uSize;
	sNotify.item.stateMask		= 0xFFFFFFFF;
	sNotify.item.cChildren		= uSub;

	lRet=SendNotify(pData,&sNotify.hdr);

	LOCK(pData);

	if (lRet) {
		TreeListEndLabelEdit(pData,1);
		return NULL;
	}

	pEntry=pData->pTreeItems[uItem];
	if (!pEntry) return NULL;

	if (iPixels<32) iPixels=32;

	UNLOCK(pData);
	SetFocus(pData->hEdit);
	LOCK(pData);

	if (pData->uEditMode) {
		sRect.top--;
		uHeight	= 160;
	} else {
		uHeight=pData->iFontHeight+4;
	}

	SetWindowTextW(pData->hEdit,pText);
	SendMessage  (pData->hEdit,WM_SETFONT,(WPARAM)hFont,0);
	SendMessage  (pData->hEdit,EM_SETSEL,0,uSize);
	SetWindowPos (pData->hEdit,HWND_TOP,sRect.left+2,sRect.top+1,iPixels,uHeight,SWP_SHOWWINDOW);
	RedrawWindow (pData->hEdit,NULL,NULL,RDW_INVALIDATE|RDW_UPDATENOW|RDW_ERASE);

	pData->uEditItem = uItem;
	pData->uEditSub	 = uSub;

	return pData->hEdit;
}


LRESULT CALLBACK HTreeListFunction(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	TreeListData   *pData;
	MSG			   *pMsg;
	LPARAM			lRet;
	TV_HITTESTINFO	sInfo;
	unsigned		uChange;
	unsigned		uFlag;
	unsigned		uVal;
	int				iPos;
	int				iMax;
	HDC				hDc;

	switch (uMsg)
	{
	case WM_CREATE:
		pData= malloc(sizeof(TreeListData));
		if(!pData)return -1;

		memset(pData,0,sizeof(TreeListData));

		pData->pTreeItems = malloc(sizeof(BaseItem*));
		if(!pData->pTreeItems) {
			free(pData);
			return -1;
		}

		pData->pItemPos  = malloc(sizeof(unsigned));
		if(!pData->pItemPos) {
			free(pData->pTreeItems);
			free(pData);
			return -1;
		}

		GlobalInit();

		pData->pItemPos  [0] = 0;
		pData->pTreeItems[0] = NULL;

		SetWindowLongPtrW(hWnd,0,(LONG_PTR)pData);
		pData->iIdent		 = DEFAULT_IDENT;
		pData->iShift		 = DEFAULT_SHIFT;
		pData->uStyle		 = GetWindowLongPtrW(hWnd,GWL_STYLE);
		pData->hSem			 = CreateSemaphore(0,1,0x70000000,0);
		pData->hWnd			 = hWnd;
		pData->cIsEnabled	 = (char)IsWindowEnabled(hWnd);

		if (!(pData->uStyle&TVS_NOTOOLTIPS))	{
			CreateToolTip(pData);
		}

		UpdateFont   (pData);
		UpdateHeight (pData);
		UpdateTreeColors (pData);
		UpdateScrollY(pData);

		if(pData->uStyle&TVS_CHECKBOXES) {
			CreateStateImageList(pData);
		}

		return 0;

	case WM_DESTROY:
		pData = GetHandle(hWnd);

		LOCK(pData);

		TreeListDeleteItem(pData,U(TVI_ROOT),0);
		if (pData->hEdit   ) DestroyWindow    (pData->hEdit   );
		if (pData->hHeader ) DestroyWindow    (pData->hHeader );
		if (pData->hToolTip) DestroyWindow    (pData->hToolTip);
		if (pData->hStates ) ImageList_Destroy(pData->hStates );
		if (pData->hImages ) ImageList_Destroy(pData->hImages );

		for(uVal=1;uVal<pData->uColumnCount;uVal++) {
			free(pData->pExtraItems[uVal-1]);
		}

		free(pData->pTreeItems);
		free(pData->pItemPos);

		UNLOCK(pData);

		CloseHandle(pData->hSem);
		memset(pData,0,sizeof(TreeListData));
		free(pData);

		GlobalDeinit();

		return 0;

	case WM_SIZE:
		pData = GetHandle(hWnd);
		uFlag = 0;

		LOCK(pData);

		uVal = LOWORD(lParam);
		if (uVal!=pData->uSizeX) {
			if (pData->uColumnCountVar) {
				unsigned 	uPos;
				HDITEMW	 	sItem;
				RECT		sRect;
				int			iDelta;
				int			iSize;
				int			iOld;
				int			iNum;

				uFlag		= 0;
				sItem.mask	= HDI_WIDTH;
				iDelta		= uVal-pData->uSizeX;
				iNum		= pData->uColumnCountVar;

				for (uPos=0; uPos < pData->uColumnCount; uPos++) {
					if (!pData->cColumnVar[uPos])
						continue;

					iOld	= pData->iColumnXpos[uPos+1];
					iOld   -= pData->iColumnXpos[uPos  ];
					iSize	= iOld+iDelta/iNum;
					uFlag	= 1;

					if (iOld <= 16 && iSize < 16) {
						iNum--;
						continue;
					}

					if (iSize<16) iSize=16;

					sItem.cxy   = iSize;
					Header_SetItemW(pData->hHeader,uPos,&sItem);

					iDelta -= iSize-iOld;
					iNum--;
				}

				iNum = UpdateColumns(pData);
				GetClientRect(hWnd,&sRect);
				sRect.left  = iNum;
				sRect.left -= pData->uScrollX;
				sRect.top   = pData->uStartPixel;
				InvalidateRect(hWnd,&sRect,FALSE);
			}

			if (uVal>pData->uSizeX) {
				RECT	sRect;

				GetClientRect (hWnd,&sRect);
				sRect.right  = uVal;
				sRect.left   = pData->uSizeX;
				InvalidateRect(hWnd,&sRect,FALSE);
			}

			pData->uSizeX							  = uVal;
			pData->iColumnXpos[pData->uColumnCount+1] = uVal;

			MoveWindow(pData->hHeader,-(int)pData->uScrollX,0,pData->uSizeX+pData->uScrollX-1,pData->uStartPixel,FALSE);
			UpdateScrollX(pData);
		}

		uVal = HIWORD(lParam);
		if (uVal!=pData->uSizeY) {
			if (uVal>pData->uSizeY) {
				RECT	sRect;

				GetClientRect (hWnd,&sRect);
				sRect.bottom  = uVal;
				sRect.top     = pData->uSizeY;
				InvalidateRect(hWnd,&sRect,FALSE);
			}

			pData->uSizeY		 =  uVal;
			pData->uSizeYsub	 = (uVal<=pData->uStartPixel)? 0:uVal-pData->uStartPixel;
			pData->uMaxEnties	 = pData->uSizeY;
			pData->uMaxEnties	-= pData->uStartPixel;
			pData->uPageEnties	 = pData->uMaxEnties;
			pData->uMaxEnties	+= pData->iRowHeight-1;
			pData->uMaxEnties   /= pData->iRowHeight;
			pData->uPageEnties  /= pData->iRowHeight;

			UpdateScrollY(pData);
		}

		UNLOCK(pData);

		if (uFlag) {
			RedrawWindow(pData->hHeader,NULL,NULL,RDW_INVALIDATE|RDW_UPDATENOW|RDW_ERASE);
		}

		return 0;

	case WM_ENABLE:
		pData = GetHandle(hWnd);
		if (!pData->cIsEnabled!=(char)wParam) {
			LOCK(pData);
			pData->cIsEnabled=(char)wParam;
			UpdateView(pData);
			if (!pData->cIsEnabled)
				UpdateToolTip(pData,0,0);
			UNLOCK(pData);
		}

	case WM_SETFOCUS:
		pData = GetHandle(hWnd);
		if (!pData->cHasFocus) {
			NMHDR	sNotify;

			LOCK(pData);
			pData->cHasFocus=1;

			if (pData->uSelectedCount<=1) {
				if (pData->uSelectedItem) {
					if (pData->uStyleEx&TVS_EX_FULLROWMARK)
						UpdateRow (pData,pData->uSelectedItem);
					else
						UpdateRect(pData,pData->uSelectedItem,pData->uSelectedSub);
				}
			} else {
				UpdateView(pData);
			}

			UNLOCK(pData);

			sNotify.code = NM_SETFOCUS;
			SendNotify(pData,&sNotify);
		}

		return 0;

	case WM_KILLFOCUS:
		pData = GetHandle(hWnd);
		if (pData->cHasFocus) {
			NMHDR	sNotify;

			LOCK(pData);
			pData->cHasFocus=0;

			if (pData->uSelectedCount<=1) {
				if (pData->uSelectedItem) {
					if (pData->uStyleEx&TVS_EX_FULLROWMARK)
						UpdateRow (pData,pData->uSelectedItem);
					else
						UpdateRect(pData,pData->uSelectedItem,pData->uSelectedSub);
				}
			} else {
				UpdateView(pData);
			}

			UNLOCK(pData);

			sNotify.code = NM_KILLFOCUS;
			SendNotify(pData,&sNotify);
		}

		return 0;

	case WM_MOUSEMOVE:										
		pData = GetHandle(hWnd);
		sInfo.hItem = 0;
		sInfo.flags = 0;

		if (!(pData->uStyle&TVS_NOTOOLTIPS)) {
			if (!pData->cIsEnabled)	{
				return 0;
			}

			LOCK(pData);

			sInfo.pt.x	= LOWORD(lParam);
			sInfo.pt.y	= HIWORD(lParam);
			TreeListHitTest(pData,&sInfo);
			UpdateToolTip(pData,U(sInfo.hItem),sInfo.flags);
			
			UNLOCK(pData);
		}

		if (pData->uStyle & TVS_TRACKSELECT) {
			if (!pData->cIsEnabled)	{
				return 0;
			}

			LOCK(pData);

			if (!pData->cHasFocus) {
				UNLOCK(pData);
				SetFocus(pData->hWnd);
				LOCK(pData);
			}

			if (!sInfo.hItem) {
				sInfo.pt.x	= LOWORD(lParam);
				sInfo.pt.y	= HIWORD(lParam);
				TreeListHitTest(pData,&sInfo);
			}

			if (sInfo.hItem && (sInfo.flags&(TVHT_ONSUBITEM|TVHT_ONSUBLABEL|TVHT_ONITEM))) {
				TreeListSetTrackItem(pData,U(sInfo.hItem),TVHT_SUBTOCOL(sInfo.flags));
			}

			UNLOCK(pData);
		}

		if (wParam&pData->uDragFlags) {
			NMTREEVIEW	sNotify;
			BaseItem   *pEntry;

			LOCK(pData);

			if (pData->uDragItem>pData->uTreeItemsMax) {
				UNLOCK(pData);
				return 0;
			}

			pEntry = pData->pTreeItems[pData->uDragItem];
			if (!pEntry) {
				UNLOCK(pData);
				return 0;
			}

			sNotify.hdr.code			= (pData->uDragFlags&MK_LBUTTON)? TVN_BEGINDRAG:TVN_BEGINRDRAG;
			sNotify.action				= (pEntry->uState&TVIS_EXPANDED)? TVE_COLLAPSE:TVE_EXPAND;
			sNotify.itemNew.mask		= TVIF_HANDLE|TVIF_PARAM|TVIF_STATE|TVIF_SUBNUMBER;
			sNotify.itemNew.hItem		= (HTREEITEM)pData->uDragItem;
			sNotify.itemNew.stateMask	= 0xFFFFFFFF;
			sNotify.itemNew.state		= pEntry->uState;
			sNotify.itemNew.lParam		= pEntry->lParam;
			sNotify.itemNew.cChildren	= pData->uDragSub;
			sNotify.itemNew.pszText		= (LPTSTR)-1;
			sNotify.itemNew.cchTextMax	= -1;
			sNotify.itemOld.mask		= 0;
			sNotify.ptDrag.x			= LOWORD(lParam);
			sNotify.ptDrag.y			= HIWORD(lParam);
			pData->uDragFlags			= 0;

			SendNotify(pData,&sNotify.hdr);

			UNLOCK(pData);
		}

		return 0;

	case WM_LBUTTONUP:										
		pData = GetHandle(hWnd);
		pData->uDragFlags &= ~MK_LBUTTON;
		return 0;

	case WM_RBUTTONUP:										
		pData = GetHandle(hWnd);
		pData->uDragFlags &= ~MK_RBUTTON;
		return 0;

	case WM_LBUTTONDOWN:									
	case WM_LBUTTONDBLCLK:
	case WM_RBUTTONDOWN:
	case WM_RBUTTONDBLCLK:
		pData	= GetHandle(hWnd);

		LOCK(pData);
		TreeListMouseClick(pData,uMsg,wParam,lParam);

		return 0;

	case WM_KEYDOWN:										
		pData	= GetHandle(hWnd);
		LOCK(pData);
		TreeListKeyDown(pData,wParam,lParam);

		return 0;

	case WM_KEYUP:										
		DefWindowProc(hWnd,uMsg,wParam,lParam);
		return 1;

	case WM_MOUSEWHEEL:										
		if (!(LOWORD(wParam)&(MK_CONTROL|MK_LBUTTON|MK_MBUTTON|MK_RBUTTON|MK_SHIFT))) {
			pData = GetHandle(hWnd);
			iPos  = pData->uScrollY;
			iPos -= (short)HIWORD(wParam)/(WHEEL_DELTA/2);
			iMax  = pData->uItemPosCount;
			iMax -=	pData->uPageEnties-1;

			if (iPos>=iMax)
				iPos=iMax;
			if (iPos<0)
				iPos=0;
			if (iPos!=(int)pData->uScrollY) {
				pData->uScrollY = iPos;
				SetScrollPos(hWnd,SB_VERT,iPos,TRUE);
				UpdateView(pData);
			}

			return 1;
		}

		return 0;

	case WM_HSCROLL:										
		pData	= GetHandle(hWnd);
		iPos	= pData->uScrollX;

		switch (LOWORD(wParam)) {
			case SB_LINEDOWN:		iPos+=16;				break;
			case SB_LINEUP:			iPos-=16;				break;
			case SB_PAGEDOWN:		iPos+=pData->uSizeX;	break;
			case SB_PAGEUP:			iPos-=pData->uSizeX;	break;
			case SB_THUMBPOSITION:	iPos =HIWORD(wParam);  	break;
			case SB_THUMBTRACK:		iPos =HIWORD(wParam);  	break;
		}

		uVal = pData->uColumnCount;
		if (uVal)
			iMax  = pData->iColumnXpos[uVal]-pData->uSizeX/2;
		else
			iMax  = pData->iMaxSizeX;
		iMax -=	pData->uSizeX/2;

		if (iPos>=iMax)
			iPos=iMax;
		if (iPos<0)
			iPos=0;
		if (iPos!=(int)pData->uScrollX) {
			pData->uScrollX = iPos;
			SetScrollPos(hWnd,SB_HORZ,iPos,TRUE);
			UpdateView(pData);

			if (pData->hHeader) {
				MoveWindow(pData->hHeader,-iPos,0,pData->uSizeX-1+iPos,pData->uStartPixel,TRUE);
			}
		}

		return 0;

	case WM_VSCROLL:										
		pData	= GetHandle(hWnd);
		iPos	= pData->uScrollY;

		switch (LOWORD(wParam)) {
		case SB_LINEDOWN:		iPos++;													break;
		case SB_LINEUP:			iPos--;													break;
		case SB_PAGEDOWN:		iPos+=(pData->uPageEnties>1)? pData->uPageEnties-1:1;	break;
		case SB_PAGEUP:			iPos-=(pData->uPageEnties>1)? pData->uPageEnties-1:1;	break;
		case SB_THUMBPOSITION:	iPos =HIWORD(wParam);  									break;
		case SB_THUMBTRACK:		iPos =HIWORD(wParam);  									break;
		}

		iMax  = pData->uItemPosCount;
		iMax -=	pData->uPageEnties-1;

		if (iPos>=iMax) iPos=iMax;
		if (iPos<0) iPos=0;
		if (iPos!=(int)pData->uScrollY) {
			pData->uScrollY = iPos;
			SetScrollPos(hWnd,SB_VERT,iPos,TRUE);
			UpdateView(pData);
		}

		return 0;

	case WM_SYSCOLORCHANGE:									
		pData	= GetHandle(hWnd);

		LOCK(pData);
		if (pData->hHeader)
			SendMessage(pData->hHeader,WM_SYSCOLORCHANGE,0,0);
		UpdateTreeColors(pData);
		UNLOCK(pData);

		return 0;

	case WM_SETFONT:										
		pData	= GetHandle(hWnd);

		LOCK(pData);
		pData->hFontN=(HFONT)wParam;
		if (UpdateFont(pData))
			UpdateView(pData);

		UNLOCK(pData);

		return 0;

	case WM_STYLECHANGED:									
		if (wParam==GWL_STYLE) {
			pData	= GetHandle(hWnd);
			lParam	= ((STYLESTRUCT*)lParam)->styleNew;

			LOCK(pData);
			uChange		   = (unsigned)lParam^pData->uStyle;
			pData->uStyle  = (unsigned)lParam;

			if (uChange&(TVS_CHECKBOXES|TVS_NONEVENHEIGHT)) {
				if (lParam&TVS_CHECKBOXES) {
					if (pData->hStates==NULL) {
						CreateStateImageList(pData);
					}
				} else {
					if (pData->iStateCheck && pData->hStates) {
						ImageList_Destroy(pData->hStates);
						pData->hStates     = NULL;
						pData->iStateCheck = 0;
					}
				}

				UpdateHeight (pData);
				UpdateScrollY(pData);
			}

			if (uChange&(TVS_HASBUTTONS|TVS_HASLINES|TVS_LINESATROOT|TVS_RTLREADING|TVS_CHECKBOXES|TVS_TRACKSELECT)) {
				UpdateView(pData);
			}

			if (uChange&TVS_NOSCROLL) {
				UpdateScrollX(pData);
				UpdateScrollY(pData);
			}

			if (uChange&TVS_NOTOOLTIPS) {
				if (pData->uStyle&TVS_NOTOOLTIPS) {
					CreateToolTip(pData);
				} else {
					UpdateToolTip(pData,0,0);
				}
			}

			UNLOCK(pData);
		}

		return 0;

	case WM_PAINT:											
		if (wParam) {
			TreeListDraw(hWnd,(HDC)wParam,NULL);
		}
		else {
			PAINTSTRUCT sPaint;

			hDc=BeginPaint  (hWnd,&sPaint);
			TreeListDraw(hWnd, hDc,&sPaint.rcPaint);
			EndPaint    (hWnd,&sPaint);
		}

		return 0;

	case WM_TIMER:											
		if (wParam==ID_TOOLTIPCHECK) {
			pData	= GetHandle(hWnd);
			if (pData->uToolTipItem) {
				LOCK(pData);

				GetCursorPos(&sInfo.pt);
				ScreenToClient(hWnd,&sInfo.pt);

				TreeListHitTest(pData,&sInfo);
				UpdateToolTip(pData,U(sInfo.hItem),sInfo.flags);
				UNLOCK(pData);
			}
		}

		return 0;

	case WM_COMMAND:										
		if (wParam==MAKELONG(3,EN_KILLFOCUS) || wParam==MAKELONG(3,EN_RETURN) || wParam==MAKELONG(3,CBN_KILLFOCUS)) {
			pData = GetHandle(hWnd);
			if (pData->uEditItem) {
				LOCK(pData);
				TreeListEndLabelEdit(pData,0);
				UNLOCK(pData);
				if (pData->uEditMode)
					if(wParam!=MAKELONG(3,EN_KILLFOCUS)) {
						SetFocus(pData->hWnd);
					}
			}
		}

		if (wParam==MAKELONG(3,EN_ESCAPE)) {
			pData = GetHandle(hWnd);
			if (pData->uEditItem) {
				LOCK(pData);
				TreeListEndLabelEdit(pData,1);
				UNLOCK(pData);
				if (pData->uEditMode)
					SetFocus(pData->hWnd);
			}
		}

		return 0;

	case WM_NOTIFY:
		if (wParam==1) {
			NMHEADERW  *pHdr=(NMHEADERW*)lParam;
			HDITEMW		sItem;
			RECT		sRect;
			int			iSize;
			int			iCol;

			if (pHdr->hdr.code==HDN_DIVIDERDBLCLICKW) {
				int		iSize;

				pData	= GetHandle(hWnd);

				LOCK(pData);

				iSize=TreeListScanColumn(pData,pHdr->iItem);
				if (iSize) {
					sItem.cxy		 = iSize;
					pHdr->hdr.code	 = HDN_ENDTRACKW;
					pHdr->pitem		 = &sItem;
				}

				UNLOCK(pData);
			}

			if (pHdr->hdr.code==HDN_TRACKW || pHdr->hdr.code==HDN_ENDTRACKW) {
				pData	= GetHandle(hWnd);
				LOCK(pData);

				iCol		= pHdr->iItem;
				sItem.mask	= HDI_WIDTH;
				sItem.cxy	= pHdr->pitem->cxy;

				Header_SetItemW(pData->hHeader,iCol,&sItem);

				iSize = UpdateColumns(pData);
				if (iSize<0x10000) {
					GetClientRect (hWnd,&sRect);
					sRect.left  = iSize;
					sRect.left -= pData->uScrollX;
					InvalidateRect(hWnd,&sRect,FALSE);
				}

				UpdateScrollX(pData);
				UNLOCK(pData);

				if (!pData->cHasFocus && pHdr->hdr.code==HDN_ENDTRACK) {
					SetFocus(pData->hWnd);
				}
			}

			if (pHdr->hdr.code==HDN_ITEMCLICK || pHdr->hdr.code==HDN_ITEMDBLCLICK) {
				pData	= GetHandle(hWnd);
				SendNotify(pData,&pHdr->hdr);
			}
		}

		return 0;

	case WM_GETDLGCODE:										
		pMsg=(MSG*)lParam;
		if (pMsg && pMsg->message==WM_KEYDOWN && pMsg->wParam==VK_RETURN) {
			return DLGC_WANTALLKEYS;
		}

		return DLGC_WANTCHARS|DLGC_WANTARROWS;

	case TVM_SETIMAGELIST:									
		pData	= GetHandle(hWnd);

		LOCK(pData);

		switch ((int)wParam) {
		case TVSIL_NORMAL:
			lRet=(LPARAM)pData->hImages;
			if (lRet==lParam)
				break;

			pData->hImages=(HIMAGELIST)lParam;
			if (!pData->hImages) {
				pData->iImagesXsize=0;
				pData->iImagesYsize=0;
			} else {
				IMAGEINFO sInfo;
				ImageList_GetImageInfo(pData->hImages,0,&sInfo);
				pData->iImagesXsize=sInfo.rcImage.right -sInfo.rcImage.left;
				pData->iImagesYsize=sInfo.rcImage.bottom-sInfo.rcImage.top;
				if (pData->hHeader)
					SendMessage(pData->hHeader,HDM_SETIMAGELIST,0,(LPARAM)pData->hImages);
			}

			if (!pData->cFixedHeight) {
				pData->iRowHeight=1;
				UpdateHeight (pData);
				UpdateScrollY(pData);
			} else {
				UpdateView(pData);
			}

			break;

		case TVSIL_STATE:
			lRet=(LPARAM)pData->hStates;
			if (lRet==lParam)
				break;

			if (pData->iStateCheck) {
				ImageList_Destroy(pData->hStates);
			}

			pData->hStates=(HIMAGELIST)lParam;
			if (!pData->hStates) {
				pData->iStatesXsize=0;
				pData->iStatesYsize=0;
			} else {
				IMAGEINFO sInfo;
				ImageList_GetImageInfo(pData->hStates,0,&sInfo);
				pData->iStatesXsize=sInfo.rcImage.right -sInfo.rcImage.left;
				pData->iStatesYsize=sInfo.rcImage.bottom-sInfo.rcImage.top;
			}

			if (!pData->cFixedHeight) {
				pData->iRowHeight=1;
				UpdateHeight (pData);
				UpdateScrollY(pData);
			} else {
				UpdateView(pData);
			}

			break;
		default:
			lRet=0;
		}


		UNLOCK(pData);

		return lRet;

	case TVM_GETIMAGELIST:									

		pData	= GetHandle(hWnd);

		switch ((int)wParam) {
		case TVSIL_NORMAL:	lRet=(LPARAM)pData->hImages; break;
		case TVSIL_STATE:	lRet=(LPARAM)pData->hImages; break;
		default:			lRet=0;
		}

		return lRet;

	case TVM_GETITEMSTATE:									

		pData = GetHandle(hWnd);
		LOCK(pData);

		if ((unsigned)wParam<=pData->uTreeItemsMax) {
			BaseItem *pEntry;

			pEntry = pData->pTreeItems[pData->uSelectedItem];
			if (!pEntry)
				lRet = 0;

			{
				lRet = pEntry->uState;
			}
		} else {
			lRet = 0;
		}

		UNLOCK(pData);

		return lRet;

	case TVM_GETITEM:										

		pData = GetHandle(hWnd);
		LOCK(pData);
		lRet  = TreeListGetItem(pData,(TV_ITEMW*)lParam);
		UNLOCK(pData);

		return lRet;

	case TVM_SETITEM:
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet = TreeListSetItem(pData,(TVITEMW*)lParam);
		UNLOCK(pData);

		return lRet;

	case TVM_INSERTITEM:									

		pData = GetHandle(hWnd);
		LOCK(pData);
		lRet  = TreeListInsertItem(pData,(TVINSERTSTRUCTW*)lParam);
		UNLOCK(pData);

		return lRet;

	case TVM_DELETEITEM:									

		pData = GetHandle(hWnd);
		LOCK(pData);
		lRet  = TreeListDeleteItem(pData,(unsigned)lParam,1);
		UNLOCK(pData);

		return lRet;

	case TVM_DELETECOLUMN:									

		pData = GetHandle(hWnd);
		LOCK(pData);
		lRet  = TreeListDeleteColumn(pData,(unsigned)wParam);
		UNLOCK(pData);

		return lRet;

	case TVM_INSERTCOLUMN:
		pData = GetHandle(hWnd);
		LOCK(pData);
		lRet  = TreeListInsertColumn(pData,(int)wParam,(TVCOLUMNW*)lParam);
		UNLOCK(pData);
		return lRet;

	case TVM_GETCOLUMNCOUNT:
		pData = GetHandle(hWnd);
		return (LRESULT)pData->uColumnCount;

	case TVM_GETHEADER:
		pData = GetHandle(hWnd);
		return (LRESULT)pData->hHeader;

	case TVM_GETEXTENDEDSTYLE:
		pData = GetHandle(hWnd);
		return pData->uStyleEx;

	case TVM_SETEXTENDEDSTYLE:								
		pData = GetHandle(hWnd);
		if(!wParam)wParam=0xFFFFFFFF;

		uVal  = pData->uStyleEx &~(unsigned)wParam;
		uVal |=	(unsigned)lParam& (unsigned)wParam;

		if (pData->uStyleEx!=uVal) {
			LOCK(pData);
			uChange = pData->uStyleEx^uVal;
			pData->uStyleEx = uVal;

			if (uChange&TVS_EX_ITEMLINES) {
				UpdateHeight (pData);
				UpdateScrollY(pData);
			}

			if (uChange&(TVS_EX_ALTERNATECOLOR|TVS_EX_SUBSELECT|TVS_EX_MULTISELECT|TVS_EX_FULLROWMARK)) {
				UpdateView(pData);
			}

			if (((uChange^lParam)&lParam)&TVS_EX_SUBSELECT) {
				TreeListSelectItem(pData,pData->uSelectedItem,0,TVC_UNKNOWN);
			}

			UNLOCK(pData);
		}

		return uVal;

	case TVM_GETINSERTMARKCOLOR:							
		wParam=TVC_INSERT; goto ColGet;
	case TVM_GETLINECOLOR:
		wParam=TVC_LINE;   goto ColGet;
	case TVM_GETTEXTCOLOR:
		wParam=TVC_TEXT;
	case TVM_GETBKCOLOR:
	ColGet:
		if(wParam<0 || wParam>=MAX_COLORS)return 0xFFFFFFFF;
		pData = GetHandle(hWnd);
		return pData->uColors[(unsigned)wParam];

	case TVM_SETINSERTMARKCOLOR:							
		wParam=TVC_INSERT; goto ColSet;
	case TVM_SETLINECOLOR:
		wParam=TVC_LINE;   goto ColSet;
	case TVM_SETTEXTCOLOR:
		wParam=TVC_TEXT;
	case TVM_SETBKCOLOR:
	ColSet:	
		if(wParam<0 || wParam>=MAX_COLORS)return 0xFFFFFFFF;

		pData = GetHandle(hWnd);

		LOCK(pData);

		lRet=pData->uColors[(unsigned)wParam];

		if ((COLORREF)lParam==TV_NOCOLOR) {
			if (pData->cColorChanged[(unsigned)wParam]) {
				pData->cColorChanged[(unsigned)wParam]=0;
				UpdateTreeColors(pData);
				UpdateView  (pData);
			}
		} else {
			pData->cColorChanged[(unsigned)wParam]=1;
			pData->uColors      [(unsigned)wParam]=(COLORREF)lParam;

			if(lRet!=lParam)UpdateView(pData);
		}

		UNLOCK(pData);

		return lRet;

	case TVM_HITTEST:										
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet=TreeListHitTest(pData,(LPTVHITTESTINFO)lParam);
		UNLOCK(pData);

		return lRet;

	case TVM_SELECTSUBITEM:									
		pData = GetHandle(hWnd);

		LOCK(pData);
		if (!(pData->uStyleEx&TVS_EX_SUBSELECT))
			wParam=0;
		lRet=TreeListSelectItem(pData,(unsigned)lParam,(unsigned)wParam,TVC_UNKNOWN);
		if(lRet>1)lRet=1;
			UNLOCK(pData);

		return lRet;

	case TVM_SELECTDROP:									
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet=TreeListSetTrackItem(pData,(unsigned)lParam,(unsigned)wParam);
		UNLOCK(pData);

		return lRet;

	case TVM_SELECTITEM:									
		pData = GetHandle(hWnd);

		LOCK(pData);

		switch (wParam) {
		case TVGN_CARET:
			lRet=TreeListSelectItem(pData,(unsigned)lParam,0,TVC_UNKNOWN);
			if (lRet>1) lRet=1;
			break;
		case TVGN_DROPHILITE:
			lRet=TreeListSetTrackItem(pData,(unsigned)lParam,0);
			break;

		case TVGN_FIRSTVISIBLE:
			lRet=TreeListEnsureVisible(pData,(unsigned)lParam,FIRST_LINE);
			lRet=(lRet<0)? 0:1;
		default:
			lRet=0;
		}

		UNLOCK(pData);

		return lRet;

	case TVM_GETCOUNT:										
		pData = GetHandle(hWnd);
		return pData->uTreeItemsCount;

	case TVM_GETINDENT:										
		pData = GetHandle(hWnd);
		return pData->iIdent;

	case TVM_SETINDENT:										
		pData = GetHandle(hWnd);
		lRet  = pData->iIdent;
		if (wParam< 5) wParam= 5;
		if (wParam>64) wParam=64;
		if (lRet!=(LPARAM)wParam) {
			LOCK(pData);
			pData->iIdent = (int)wParam;
			UpdateView(pData);
			UNLOCK(pData);
		}

		return lRet;

	case TVM_GETITEMHEIGHT:									
		pData = GetHandle(hWnd);
		return pData->iRowHeight;

	case TVM_SETITEMHEIGHT:									
		pData = GetHandle(hWnd);
		lRet  = pData->iRowHeight;

		if (wParam==-1) {
			LOCK(pData);
			pData->cFixedHeight=0;
			UpdateHeight (pData);
			UpdateScrollY(pData);
			UNLOCK(pData);
			return lRet;
		}

		if (wParam&1 )
			if (!(pData->uStyleEx&TVS_NONEVENHEIGHT))
				wParam--;
		if (wParam<  1)
			wParam=  1;
		if (wParam>256)
			wParam=256;

		if (lRet!=(LPARAM)wParam) {
			LOCK(pData);
			pData->cFixedHeight = 1;
			pData->iRowHeight   = (int)wParam;
			UpdateView(pData);
			UNLOCK(pData);
		}

		return lRet;

	case TVM_GETVISIBLECOUNT:								
		pData = GetHandle(hWnd);
		return (pData->uSizeY-pData->uStartPixel)/pData->iRowHeight;

	case TVM_ENSUREVISIBLE:									
		pData = GetHandle(hWnd);
		LOCK(pData);
		lRet=TreeListEnsureVisible(pData,(unsigned)lParam,(int)wParam);
		UNLOCK(pData);

		return lRet;

	case TVM_GETNEXTITEM:									
		pData = GetHandle(hWnd);
		LOCK(pData);
		lRet=TreeListGetNextItem(pData,(unsigned)lParam,(int)wParam);
		UNLOCK(pData);

		return lRet;

	case TVM_GETITEMRECT:									
		pData = GetHandle(hWnd);
		LOCK(pData);
		uVal=*(unsigned*)lParam;
		lRet=TreeListGetItemRect(pData,uVal,(unsigned)wParam,(RECT*)lParam);
		UNLOCK(pData);

		return lRet;

	case TVM_EXPAND:										
		pData = GetHandle(hWnd);
		LOCK(pData);
		lRet=0;

		if ((unsigned)lParam<=pData->uTreeItemsMax) {
			BaseItem *pEntry;
			POINT	  sPoint;

			pEntry = pData->pTreeItems[(unsigned)lParam];
			if (pEntry) {
				sPoint.x = 0;
				sPoint.y = 0;

				switch (wParam&0x0F) {
				case TVE_COLLAPSE:
					if (pEntry->uState&TVIS_EXPANDED) {
						lRet=(TreeListToggleItem(pData,lParam,0))? 0:1;
					} else {
						if (pEntry->uState&TVIS_EXPANDPARTIAL) {
							pEntry->uState &= ~TVIS_EXPANDPARTIAL;
							UpdateRect(pData,(unsigned)lParam,0);
						}

						lRet=1;
					}

					if (wParam&TVE_COLLAPSERESET) {
						pEntry->uState &= TVIS_EXPANDEDONCE;

						while (pEntry->uLastChild) {
							uVal = (pEntry->uLastChild==pEntry->uFirstChild)? 1:0;
							if (!TreeListDeleteItem(pData,pEntry->uLastChild,uVal))
								break;
							pEntry = pData->pTreeItems[(unsigned)lParam];
						}
					}

					break;

				case TVE_EXPAND:
					if (!(pEntry->uState&TVIS_EXPANDED)) {
						uVal = (wParam&TVE_EXPANDPARTIAL)? TVIS_EXPANDPARTIAL:0;
						lRet = (TreeListToggleItem(pData,(unsigned)lParam,uVal))? 0:1;
					} else {
						if (wParam&TVE_EXPANDPARTIAL) {
							if (!(pEntry->uState&TVIS_EXPANDPARTIAL)) {
								pEntry->uState |= TVIS_EXPANDPARTIAL;
								UpdateRect(pData,(unsigned)lParam,0);
							}
						} else {
							if (pEntry->uState&TVIS_EXPANDPARTIAL) {
								pEntry->uState &= ~TVIS_EXPANDPARTIAL;
								UpdateRect(pData,(unsigned)lParam,0);
							}
						}

						lRet = 1;
					}

					break;

				case TVE_TOGGLE:
					if (pEntry->uState&TVIS_EXPANDPARTIAL) {
						if (pEntry->uState&TVIS_EXPANDED) {
							pEntry->uState &= ~TVIS_EXPANDPARTIAL;
							UpdateRect(pData,(unsigned)lParam,0);
							lRet = 1;
							break;
						}
					} else {
						lRet = 1;
					}

					lRet=(TreeListToggleItem(pData,lParam,0))? 0:1;
					break;
				}
			}
		}

		UNLOCK(pData);

		return lRet;

	case TVM_SETINSERTMARK:									
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet=TreeListSetInsertMark(pData,(unsigned)lParam,(int)wParam);
		UNLOCK(pData);

		return lRet;

	case TVM_SETITEMBKCOLOR:								
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet=TreeListSetItemColor(pData,(unsigned)wParam&0xFFFFFF,wParam>>24,(COLORREF)lParam,0);
		UNLOCK(pData);

		return lRet;

	case TVM_SETITEMTEXTCOLOR:								
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet=TreeListSetItemColor(pData,(unsigned)wParam&0xFFFFFF,wParam>>24,(COLORREF)lParam,1);
		UNLOCK(pData);

		return lRet;

	case TVM_GETITEMBKCOLOR:								
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet=TreeListGetItemColor(pData,(unsigned)wParam,(unsigned)lParam,0);
		UNLOCK(pData);

		return lRet;

	case TVM_GETITEMTEXTCOLOR:
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet=TreeListGetItemColor(pData,(unsigned)wParam,(unsigned)lParam,1);
		UNLOCK(pData);

		return lRet;

	case TVM_SORTCHILDRENCB:								
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet=TreeListSortItemsCb(pData,(TV_SORTCB*)lParam,(int)wParam);
		UNLOCK(pData);

		return lRet;

	case TVM_SORTCHILDREN:									
		pData = GetHandle(hWnd);

		LOCK(pData);
		lRet=TreeListSortItems(pData,(unsigned)lParam,(int)wParam);
		UNLOCK(pData);

		return lRet;

	case TVM_GETITEMOFROW:
		pData = GetHandle(hWnd);

		LOCK(pData);
		if ((unsigned)lParam<pData->uItemPosCount)
			lRet=pData->pItemPos[(unsigned)lParam];
		else
			lRet=0;
		UNLOCK(pData);

		return lRet;

	case TVM_GETROWOFITEM:									
		pData = GetHandle(hWnd);

		LOCK(pData);
		if ((unsigned)lParam<=pData->uTreeItemsMax) {
			BaseItem *pEntry;

			pEntry=pData->pTreeItems[(unsigned)lParam];
			if (!pEntry)
				lRet = -1;
			else
				lRet = pEntry->uShowPos-1;
		} else {
			lRet=-1;
		}
		UNLOCK(pData);

		return lRet;

	case TVM_EDITLABEL:
		pData = GetHandle(hWnd);
		if(!(pData->uStyle&TVS_EDITLABELS))return 0;

		LOCK(pData);
		lRet=(LRESULT)TreeListEditLabel(pData,(unsigned)lParam,(unsigned)wParam);
		UNLOCK(pData);

		return lRet;

	case TVM_GETEDITCONTROL:
		pData = GetHandle(hWnd);
		return (LRESULT)pData->hEdit;

	case TVM_GETTOOLTIPS:
		pData = GetHandle(hWnd);
		return (LRESULT)pData->hToolTip;

	case TVM_SETTOOLTIPS:
		pData = GetHandle(hWnd);
		lRet  = (LRESULT)pData->hToolTip;
		pData->hToolTip = (HWND)wParam;
		return lRet;

	case TVM_SETUSERDATASIZE:
		if (lParam<0 || lParam>0x1000000)
			return -1;
		pData = GetHandle(hWnd);

		LOCK(pData);

		if (pData->uTreeItemsCount>0) {
			lRet = 0;
		} else {
			pData->uUserDataSize = lParam;
			lRet = lParam;
		}

		UNLOCK(pData);

		return lRet;

	case TVM_GETUSERDATASIZE:
		pData = GetHandle(hWnd);
		return pData->uUserDataSize;

	case TVM_GETUSERDATA:
		pData = GetHandle(hWnd);

		if (pData->uUserDataSize && (unsigned)lParam<=pData->uTreeItemsMax) {
			BaseItem   *pEntry;

			pEntry = pData->pTreeItems[(unsigned)lParam];
			if(pEntry)return (LRESULT)(pEntry+1);
		}

		return 0;

	case TVM_SETCOLUMN:
		pData = GetHandle(hWnd);
		if (pData->hHeader) {
			HDITEMW		sItem;
			TVCOLUMNW  *pCol=(TVCOLUMNW*)lParam;

			sItem.mask = 0;
			if (pCol->mask&TVCF_FMT  ) {
				sItem.mask |= HDI_FORMAT;
				sItem.fmt   = pCol->fmt;
			}
			if (pCol->mask&TVCF_IMAGE) {
				sItem.mask|=HDI_IMAGE;
				sItem.iImage = pCol->iImage;
			}
			if (pCol->mask&TVCF_WIDTH) {
				sItem.mask |= HDI_WIDTH;
				sItem.cxy   = pCol->cx;
			}
			if (pCol->mask&TVCF_TEXT) {
				sItem.mask|=HDI_TEXT;
				sItem.pszText=pCol->pszText;
				sItem.cchTextMax=pCol->cchTextMax;
			}

			lRet=SendMessage(pData->hHeader,HDM_SETITEM,wParam,(LPARAM)&sItem);
		} else {
			lRet =0;
		}

		return lRet;

	case TVM_GETCOLUMN:
		pData = GetHandle(hWnd);
		if (pData->hHeader) {
			HDITEMW		sItem;
			TVCOLUMNW  *pCol=(TVCOLUMNW*)lParam;

			sItem.mask = 0;
			if(pCol->mask&TVCF_FMT  ){sItem.mask|=HDI_FORMAT;}
			if(pCol->mask&TVCF_IMAGE){sItem.mask|=HDI_IMAGE; }
			if(pCol->mask&TVCF_WIDTH){sItem.mask|=HDI_WIDTH; }
			if(pCol->mask&TVCF_TEXT ){sItem.mask|=HDI_TEXT ;sItem.pszText=pCol->pszText;  sItem.cchTextMax=pCol->cchTextMax;}

			lRet=SendMessage(pData->hHeader,HDM_GETITEM,wParam,(LPARAM)&sItem);

			pCol->mask = 0;

			if (sItem.mask|=HDI_FORMAT) {pCol->mask|=TVCF_FMT  ; pCol->fmt    =sItem.fmt;    }
			if (sItem.mask|=HDI_IMAGE ) {pCol->mask|=TVCF_IMAGE; pCol->iImage =sItem.iImage; }
			if (sItem.mask|=HDI_WIDTH ) {pCol->mask|=TVCF_WIDTH; pCol->cx     =sItem.cxy;    }
			if (sItem.mask|=HDI_TEXT  ) {pCol->mask|=TVCF_TEXT ; pCol->pszText=sItem.pszText;  pCol->cchTextMax=sItem.cchTextMax;}
		} else {
			lRet =0;
		}

		return lRet;

	case TVM_SETCOLUMNWIDTH:
		pData = GetHandle(hWnd);
		if(pData->hHeader) {
			HDITEMW sItem;

			sItem.mask = HDI_WIDTH;
			sItem.cxy  = (int)lParam;
			lRet=SendMessage(pData->hHeader,HDM_SETITEM,wParam,(LPARAM)&sItem);
		} else {
			lRet=0;
		}

		return lRet;

	case TVM_GETCOLUMNWIDTH:								
		pData = GetHandle(hWnd);
		if(pData->hHeader) {
			HDITEMW sItem;

			sItem.mask = HDI_WIDTH;
			sItem.cxy  = (int)lParam;
			SendMessage(pData->hHeader,HDM_GETITEM,wParam,(LPARAM)&sItem);
			lRet = sItem.cxy;
		} else {
			lRet=0;
		}
		return lRet;

	case TVM_CREATEDRAGIMAGE:
		pData = GetHandle(hWnd);
		LOCK(pData);
		lRet = (LRESULT)CreateDragImage(pData,(unsigned)lParam,(unsigned)wParam);
		UNLOCK(pData);
		return 0;
	}

	return DefWindowProc(hWnd,uMsg,wParam,lParam);
}

static void TreeListDraw(HWND hWnd,HDC hDc,RECT *pRect)
{
	COLORREF		uEvColor;
	COLORREF		uBkColor;
	COLORREF		uBxColor;
	COLORREF		uBtColor;
	COLORREF		uOdColor;
	COLORREF		uFrColor;
	COLORREF		uInColor;
	COLORREF		uOldColor;
	COLORREF		uOutColor;
	COLORREF		uTempColor;
	HRGN			hRgnMain;
	HRGN			hRgn[MAX_COLUMNS+1];
	RECT			sRect;
	RECT			sArea;
	RECT			sButton;
	TreeListData   *pData;
	BaseItem	   *pTemp;
	BaseItem	   *pEntry;
	ExtraItem	   *pExtra;
	LPCWSTR			pText;
	unsigned		uTextSize;
	unsigned		uRgnCount;
	unsigned		uStyleEx;
	unsigned		uStyle;
	unsigned		uExtra;
	unsigned		uMark;
	unsigned		uItem;
	unsigned		uBits;
	unsigned		uPos;
	unsigned		uMax;
	int			   *pOffsets;
	int				iRnType[MAX_COLUMNS+1];
	int				iXscroll;
	int				iHeight;
	int				iDelta;
	int				iImage;
	int				iIdent;
	int				iShift;
	int				iStart;
	int				iLevel;
	int				iLast;
	int				iXpos;
	int				iYpos;
	int				iMaxX;
	int				iAdd;
	int				i;


	pData=GetHandle(hWnd);

	LOCK(pData);

	GetClientRect(hWnd,&sRect);

	if(!pRect)pRect=&sRect;

	iXscroll = -(int)pData->uScrollX;
	pOffsets = pData->iColumnXpos;
	hRgnMain = CreateRectRgn(pRect->left,pRect->top,pRect->right,pRect->bottom);

	uMax = pData->uColumnCount;
	if (!uMax) {
		hRgn   [  0 ] = CreateRectRgn(sRect.left,sRect.top,sRect.right,sRect.bottom);
		iRnType[  0 ] = CombineRgn(hRgn[0],hRgn[0],hRgnMain,RGN_AND);
		uRgnCount	  = 1;
	} else {
		for (uPos=0;uPos<uMax;uPos++) {
			hRgn   [uPos] = CreateRectRgn(sRect.left+pOffsets[uPos]+iXscroll,sRect.top,sRect.left+pOffsets[uPos+1]+iXscroll,sRect.bottom);
			iRnType[uPos] = CombineRgn(hRgn[uPos],hRgn[uPos],hRgnMain,RGN_AND);
		}

		hRgn   [uPos] = CreateRectRgn(sRect.left+pOffsets[uPos]+iXscroll,sRect.top,sRect.right,sRect.bottom);
		iRnType[uPos] = CombineRgn(hRgn[uPos],hRgn[uPos],hRgnMain,RGN_AND);

		uRgnCount = uMax;
	}

	iHeight		  = pData->iRowHeight;
	uStyleEx 	  = pData->uStyleEx;
	uStyle	 	  = pData->uStyle;
	iIdent	 	  = pData->iIdent;
	iShift	 	  = pData->iShift;
	uPos	 	  = pData->uScrollY;
	uMax	 	  = pData->uMaxEnties+uPos;


	if (iRnType[0]==NULLREGION) 
		iMaxX = pData->iMaxSizeX;
	else
		iMaxX = pData->iColumnXpos[1];

	if (uStyleEx&TVS_EX_ITEMLINES) {
		iHeight--;
	}

	if (uMax>pData->uItemPosCount) {
		uMax=pData->uItemPosCount;
	}

	if (pData->cIsEnabled) {
		uBkColor  = pData->uColors[TVC_BK   ];
		uOdColor  = pData->uColors[TVC_ODD  ];
		uFrColor  = pData->uColors[TVC_FRAME];
		if (pData->uStyleEx&(TVS_EX_ITEMLINES|TVS_EX_ALTERNATECOLOR))
			uEvColor = pData->uColors[TVC_EVEN];
		else
			uEvColor = uBkColor;
	} else {
		uBxColor  = GetSysColor(COLOR_WINDOW);
		uBkColor  = GetSysColor(COLOR_SCROLLBAR);
		uFrColor  = ((((uBkColor    )&0xFF)*2  +((uBxColor    )&0xFF))/3);
		uFrColor |= ((((uBkColor>> 8)&0xFF)*2  +((uBxColor>> 8)&0xFF))/3)<<8;
		uFrColor |= ((((uBkColor>>16)&0xFF)*2  +((uBxColor>>16)&0xFF))/3)<<16;
		uOdColor  = ((((uBkColor>> 1)&0x7F7F7F)+((uFrColor>> 1)&0x7F7F7F)));
		uEvColor  =     uBkColor;
	}

	uInColor = pData->uColors[TVC_LINE];
	uBtColor = pData->uColors[TVC_BOX ];
	uBxColor = uBkColor;
	iStart	 = 0;
	iLast	 = 0;

	sArea.top = sRect.top+pData->uStartPixel;
	SelectObject(hDc,pData->hFontN);
	SelectObject(hDc,hPatternPen);
	SetBkColor	(hDc,uBkColor);
	SetBkMode   (hDc,TRANSPARENT);
	SetTextAlign(hDc,TA_LEFT|TA_TOP);
	SetTextColor(hDc,pData->uColors[TVC_TEXT]);

	for (;uPos<uMax;uPos++)	{
		uItem = pData->pItemPos[uPos];

		pEntry	= pData->pTreeItems[uItem];
		if (!pEntry)
			break;

		if ((uStyleEx&TVS_EX_FULLROWMARK) && (pEntry->uState&TVIS_SELECTED)) {
			if (uStyleEx&TVS_EX_ALTERNATECOLOR)
				uOutColor = (uPos&1)? pData->uColors[TVC_MARKODD]:pData->uColors[TVC_MARKEVEN];
			else
				uOutColor = pData->uColors[TVC_MARK];

			uMark &= ~TVIS_BKCOLOR;
		} else if (uStyleEx&TVS_EX_ALTERNATECOLOR) {
			uOutColor = (uPos&1)? uOdColor:uEvColor;
			uMark     = 0xFFFFFFFF;
		} else {
			uOutColor = uBxColor;
			uMark     = 0xFFFFFFFF;
		}

		sArea.bottom	= sArea.top+pData->iRowHeight;
		sArea.left		= iXscroll;
		iLevel			= pEntry->uLevel;

		if (iRnType[0]==NULLREGION) {
			goto ExtraDraw;
		}

		uBits		=  pEntry->uState&0xFFFF;
		uBits      |=  pEntry->bFlags<<16;
		uBits      &=  uMark;
		iImage		= (uBits&LVIS_SELECTED)? pEntry->iSelectedImage:pEntry->iImage;
		pText		=  pEntry->pText;
		uTextSize	=  pEntry->uTextSize;

		if (pData->uSelectedSub && uItem==pData->uSelectedItem) {
			uBits &= ~TVIS_SELECTED;
		}

		if (pEntry->bCallback) {
			CallbackEntry(pData,pEntry,uItem,pEntry->bCallback,&iImage,&uTextSize,&pText);

			pEntry	= pData->pTreeItems[uItem];
			if (!pEntry)
				break;
		}

		SelectObject(hDc,hRgn[0] );
		SetBkColor  (hDc,uBkColor);

		if ((uStyle&TVS_HASLINES) && (iLevel>0 || (uStyle&TVS_LINESATROOT))) {
			sArea.right = sArea.left+1;						
			ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);
			sArea.left++;

			if (!(uStyle&TVS_LINESATROOT))
				iLevel--;

			if (iLevel>0) {
				pTemp		= pData->pTreeItems[pEntry->uParent];
				sArea.left += iIdent*iLevel;

				for(i=iLevel;i>0;i--) {
					sArea.right = sArea.left;
					sArea.left -= iIdent;

					ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);

					iXpos = sArea.left+iShift;

					if (pTemp) {
						if (pTemp->uNextItem) {
							MoveToEx(hDc,iXpos,sArea.top|1,NULL);
							LineTo  (hDc,iXpos,sArea.bottom);
						}

						pTemp = pData->pTreeItems[pTemp->uParent];
					}
				}
			}

			sArea.left += iIdent*iLevel;
		} else {												
			if (!(uStyle&TVS_LINESATROOT))
				iLevel--;
			if (iLevel>0) {
				sArea.right = sArea.left+iIdent*iLevel;
				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);
				sArea.left += sArea.right;
			}
		}

		if (iLevel<0)
			goto NoRootLines;

		if (uStyle&TVS_HASBUTTONS) {
			sArea.right = sArea.left+iIdent;
			iXpos		= sArea.left+iShift;
			iYpos		= sArea.top+pData->iRowHeight/2;

			ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);

			if (uStyle&TVS_HASLINES) {
				MoveToEx(hDc,iXpos,sArea.top|1,NULL);

				if (pEntry->uNextItem)
					LineTo(hDc,iXpos,sArea.bottom);
				else
					LineTo(hDc,iXpos,iYpos+1);

				MoveToEx(hDc,iXpos+1+(iYpos&1),iYpos,NULL);
				LineTo  (hDc,sArea.right      ,iYpos);
			}

			if (pEntry->bFlags&TVIX_HASBUTTON) {
				sButton.left    = iXpos-4;
				sButton.top	    = iYpos-4;
				sButton.right   = iXpos+5;
				sButton.bottom  = iYpos+5;

				SetBkColor(hDc,uBtColor);
				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sButton,NULL,0,NULL);

				sButton.left   += 1;
				sButton.top	   += 1;
				sButton.right  -= 1;
				sButton.bottom -= 1;

				SetBkColor(hDc,uBkColor);
				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sButton,NULL,0,NULL);

				sButton.left    = iXpos-2;
				sButton.top	    = iYpos  ;
				sButton.right   = iXpos+3;
				sButton.bottom  = iYpos+1;

				SetBkColor(hDc,uInColor);
				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sButton,NULL,0,NULL);

															
				if ((uBits^TVIS_EXPANDED)&(TVIS_EXPANDED|TVIS_EXPANDPARTIAL)) {
					sButton.left    = iXpos  ;
					sButton.top	    = iYpos-2;
					sButton.right   = iXpos+1;
					sButton.bottom  = iYpos+3;
					ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sButton,NULL,0,NULL);
				}

				SetBkColor(hDc,uBkColor);
			}

			sArea.left += iIdent;
		} else if (uStyle&TVS_HASLINES)	{
			sArea.right = sArea.left+iIdent;
			ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);

			iYpos	= sArea.top+pData->iRowHeight/2;
			iXpos	= sArea.left+iShift;
			MoveToEx(hDc,iXpos,sArea.top|1,NULL);

			if(pEntry->uNextItem)
				LineTo(hDc,iXpos,sArea.bottom);
			else
				LineTo(hDc,iXpos,iYpos+1);

			MoveToEx(hDc,iXpos+1+(iYpos&1),iYpos,NULL);
			LineTo  (hDc,sArea.right      ,iYpos);

			sArea.left += iIdent;
		}

NoRootLines:
		if (uStyleEx&TVS_EX_ITEMLINES) {
			iAdd		= 1;
			sArea.right = sArea.left+1;

			if (iLevel>=0) {
				SetBkColor(hDc,uFrColor);
				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);

				sArea.left++;
				sArea.bottom--;
				iStart=sArea.left;
			} else {
				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);

				sArea.left++;
				sArea.bottom--;
				iStart=sArea.left-1;
			}
		} else {
			iAdd = 0;
		}

		SetBkColor(hDc,(uBits&TVIS_BKCOLOR)? pEntry->uColorBk:uOutColor);

		if (pData->hStates) {
			sArea.right		= sArea.left+pData->iStatesXsize;
			iYpos			= sArea.top+(iHeight-pData->iStatesYsize)/2;
			i				= (uBits&LVIS_STATEIMAGEMASK)>>12;

			sArea.right    += iAdd;
			ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);
			sArea.left     += iAdd;
			ImageList_Draw(pData->hStates,i,hDc,sArea.left,iYpos,ILD_TRANSPARENT);

			sArea.left += pData->iStatesXsize;
			iAdd		= 0;
		}

		if (pData->hImages && iImage!=TV_NOIMAGE) {
			sArea.right		= sArea.left+pData->iImagesXsize;
			iYpos			= sArea.top+(iHeight-pData->iImagesYsize)/2;
			pEntry->bFlags |= TVIX_HASIMAGE;

			SelectObject(hDc,(uBits&TVIS_BOLD)? pData->hFontB:pData->hFontN);
			sArea.right    += iAdd;
			ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);
			sArea.left     += iAdd;
			ImageList_Draw(pData->hImages,iImage,hDc,sArea.left,iYpos,ILD_TRANSPARENT|(uBits&(TVIS_OVERLAYMASK|LVIS_CUT)));

			sArea.left += pData->iImagesXsize;
			iAdd		= 0;
		} else {
			pEntry->bFlags &= ~TVIX_HASIMAGE;
		}

		sArea.right = pData->iColumnXpos[1];				
		iYpos		= sArea.top+(iHeight-pData->iFontHeight)/2;

		if (uBits&(TVIS_SELECTED|TVIS_DROPHILITED|TVIS_UNTERLINE|TVIS_TRACKED|TVIS_TEXTCOLOR|TVIS_FOCUSED)) {
			ExtTextOutW(hDc,0,0,ETO_OPAQUE|ETO_CLIPPED,&sArea,NULL,0,NULL);

			sButton.top		= iYpos;
			sButton.left    = sArea.left+4;
			sButton.right	= sArea.right;
			sButton.bottom	= sArea.bottom;

			if (!uTextSize) {
				sButton.right-=2;
				sButton.bottom--;
				pEntry->iTextPixels = 0;
			} else {
				DrawTextW(hDc,pText,uTextSize,&sButton,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX|DT_CALCRECT);
				pEntry->iTextPixels = sButton.right-sButton.left;
			}

			if ((uBits&TVIS_SELECTED) && pData->cHasFocus && uItem==pData->uSelectedItem && !pData->uSelectedSub) {
				uTempColor=GetSysColor(COLOR_HIGHLIGHTTEXT);
				SelectObject(hDc,GetSysColorBrush(COLOR_HIGHLIGHT));
				Rectangle   (hDc,sButton.left-2,sButton.top-1,sButton.right+2,sButton.bottom+1);
			} else {
				if (uBits&TVIS_DROPHILITED) {
					uTempColor =     GetSysColor     (COLOR_HIGHLIGHTTEXT);
					SelectObject(hDc,GetSysColorBrush(COLOR_HIGHLIGHT   ));
					SelectObject(hDc,GetStockObject  (NULL_PEN          ));
					Rectangle   (hDc,sButton.left-2,sButton.top-1,sButton.right+2,sButton.bottom+1);
				} else if (uBits&TVIS_SELECTED) {
					if (pData->cHasFocus) {
						uTempColor =     GetSysColor     (COLOR_HIGHLIGHTTEXT);
						SelectObject(hDc,GetSysColorBrush(COLOR_HIGHLIGHT));
						SelectObject(hDc,GetStockObject  (NULL_PEN       ));
					} else {
						uTempColor = pData->uColors[TVC_TEXT];
						SelectObject(hDc,GetSysColorBrush(COLOR_MENU));
						SelectObject(hDc,GetStockObject  (NULL_PEN  ));
					}

					Rectangle(hDc,sButton.left-2,sButton.top-1,sButton.right+2,sButton.bottom+1);
				} else {
					if (uBits&TVIS_TRACKED  )      uTempColor = pData ->uColors[TVC_TRACK];
					else if (uBits&TVIS_TEXTCOLOR) uTempColor = pEntry->uColorText;
					else						   uTempColor = pData ->uColors[TVC_TEXT ];
					sButton.right--;
					sButton.left --;
				}

				SelectObject(hDc,hPatternPen);

				if (uBits&TVIS_FOCUSED) {
					SelectObject(hDc,GetStockObject(NULL_BRUSH));
					Rectangle   (hDc,sButton.left-2,sButton.top-1,sButton.right+2,sButton.bottom+1);
				}
			}

			SetTextColor(hDc,uTempColor);
			sButton.left+=pData->iFontOff;
			DrawTextW(hDc,pText,uTextSize,&sButton,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX);

			if (uBits&(TVIS_UNTERLINE|TVIS_TRACKED)) {
				sButton.left   -= pData->iFontOff;
				sButton.right  -= pData->iFontOff+1;
				sButton.top	   += pData->iFontLine;
				sButton.bottom  = sButton.top+1;
				uOldColor=SetBkColor(hDc,uTempColor);
				ExtTextOutW(hDc,0,0,ETO_OPAQUE|ETO_CLIPPED,&sButton,NULL,0,NULL);
				SetBkColor(hDc,uOldColor);
			}

			SetTextColor(hDc,pData->uColors[TVC_TEXT]);
		} else {
			if (!pEntry->iTextPixels) {
				sButton.top		= iYpos;
				sButton.left    = sArea.left+4;
				sButton.right	= sArea.right;
				sButton.bottom	= sArea.bottom;

				DrawTextW(hDc,pText,uTextSize,&sButton,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX|DT_CALCRECT);

				pEntry->iTextPixels = sButton.right-sButton.left;
			}

			ExtTextOutW(hDc,sArea.left+2,iYpos,ETO_OPAQUE|ETO_CLIPPED,&sArea,pText,uTextSize,NULL);
		}

		i  = sArea.left-iXscroll;
		i += pEntry->iTextPixels+5;
		if(i>iMaxX)iMaxX=i;

		if (uStyleEx&TVS_EX_ITEMLINES) {
			SetBkColor(hDc,uFrColor);

			if(iLast>iStart) {
				sArea.top--;
				sArea.bottom = sArea.top+1;
				sArea.left	 = iStart-1;
				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);
				sArea.top++;
			}

			iLast		  = iStart;
			sArea.top    += iHeight;
			sArea.left	  = iStart;
			sArea.bottom  = sArea.top+1;

			ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);

			sArea.top    -= iHeight;
		}

ExtraDraw:

		for (uExtra=1;uExtra<=pData->uColumnCount;uExtra++)	{
			GetRgnBox(hRgn[uExtra],&sButton);

			if (iRnType[uExtra]==NULLREGION)
				continue;

			SelectObject(hDc,hRgn[uExtra]);

			sArea.left  = pData->iColumnXpos[uExtra];
			sArea.left += iXscroll;

			if (uStyleEx&TVS_EX_ITEMLINES) {
				SetBkColor(hDc,uFrColor);

				sArea.right   = sArea.left+1;
				sArea.bottom += 1;

				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);

				sArea.left   += 1;
				sArea.top    += iHeight;
				sArea.bottom  = sArea.top+1;
				sArea.right	  = pData->iColumnXpos[uExtra+1];
				if(uExtra<pData->uColumnCount)sArea.right+=iXscroll;

				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);

				sArea.top    -= iHeight;
				sArea.bottom -= 1;
				iAdd		  = 1;
			}

			if (sArea.left>(int)pData->uSizeX)
				break;			

			sArea.right	 = pData->iColumnXpos[uExtra+1];

			if (uExtra<pData->uColumnCount) {
				sArea.right += iXscroll;
				pExtra       = pData->pExtraItems[uExtra-1][uItem];
			} else {
				pExtra = 0;
				if(uExtra<pData->uColumnCount)sArea.right+=iXscroll;
			}

			if (!pExtra) {
				SetBkColor(hDc,uOutColor);
				ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);
			} else {
				iImage		=  pExtra->iImage;
				pText		=  pExtra->pText;
				uTextSize	=  pExtra->uTextSize;
				uBits		= (pExtra->uState&0xFFFF);
				uBits	   |= (pExtra->bFlags<<16);
				uBits	   |=  pEntry->uState&TVIS_BASEFLAGS;
				uBits	   &=  uMark;

				if (uExtra!=pData->uSelectedSub) {
					uBits &= ~TVIS_SELECTED;
				}

				if (pExtra->bCallback) {
					CallbackExtra(pData,pEntry,pExtra,uItem,uExtra,pExtra->bCallback,&iImage,&uTextSize,&pText);
					pExtra = pData->pExtraItems[uExtra-1][uItem];
					if (!pExtra) break;
				}

				SetBkColor(hDc,(uBits&TVIS_BKCOLOR)? pExtra->uColorBk:uOutColor);

				if(pData->hImages && iImage>TV_NOIMAGE)	{
					sArea.right     = sArea.left+pData->iImagesXsize+2;
					iYpos		    = sArea.top+(iHeight-pData->iImagesYsize)/2;
					pExtra->bFlags |= TVIX_HASIMAGE;

					SelectObject(hDc,(uBits&TVIS_BOLD)? pData->hFontB:pData->hFontN);
					ExtTextOutW(hDc,0,0,ETO_OPAQUE,&sArea,NULL,0,NULL);
					ImageList_Draw(pData->hImages,iImage,hDc,sArea.left+1,iYpos,ILD_TRANSPARENT|(uBits&(TVIS_OVERLAYMASK|LVIS_CUT)));

					sArea.left  += pData->iImagesXsize+1;
					sArea.right  = pData->iColumnXpos[uExtra+1];
					sArea.right += iXscroll;
				} else {
					pExtra->bFlags &= ~TVIX_HASIMAGE;
				}

				iYpos = sArea.top+(iHeight-pData->iFontHeight)/2;
				SelectObject(hDc,(uBits&TVIS_BOLD)? pData->hFontB:pData->hFontN);
															
				if (uBits&(TVIS_SELECTED|TVIS_DROPHILITED|TVIS_UNTERLINE|TVIS_TRACKED|TVIS_TEXTCOLOR|TVIS_FOCUSED)) {
					ExtTextOutW(hDc,0,0,ETO_OPAQUE|ETO_CLIPPED,&sArea,NULL,0,NULL);

					sButton.top		= iYpos;
					sButton.left    = sArea.left+4;
					sButton.right	= sArea.right;
					sButton.bottom	= sArea.bottom;

					if (!uTextSize) {
						sButton.left--;
						sButton.right-=2;
						sButton.bottom--;
						pExtra->iTextPixels = 0;
					} else {
						DrawTextW(hDc,pText,uTextSize,&sButton,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX|DT_CALCRECT);
						pExtra->iTextPixels = sButton.right-sButton.left;
					}

					switch (pData->cColumnAlign[uExtra])	{
					case DT_CENTER:
						iDelta	= sArea  .right-sArea  .left;
						iDelta -= sButton.right-sButton.left;
						iDelta -= 6;
						iDelta /= 2;	
						sButton.right += iDelta;
						sButton.left  += iDelta;
						break;
					case DT_RIGHT:
						iDelta	= sArea  .right-sArea  .left;
						iDelta -= sButton.right-sButton.left;
						iDelta -= 6;
						sButton.right += iDelta;
						sButton.left  += iDelta;
						break;
					}

					if((uBits&TVIS_SELECTED) && pData->cHasFocus && uItem==pData->uSelectedItem && uExtra==pData->uSelectedSub) {
						uTempColor = GetSysColor(COLOR_HIGHLIGHTTEXT);
						SelectObject(hDc,GetSysColorBrush(COLOR_HIGHLIGHT));
						Rectangle   (hDc,sButton.left-2,sButton.top-1,sButton.right+2,sButton.bottom+1);
					} else {
						if (uBits&TVIS_DROPHILITED) {
							uTempColor =     GetSysColor     (COLOR_HIGHLIGHTTEXT);
							SelectObject(hDc,GetSysColorBrush(COLOR_HIGHLIGHT   ));
							SelectObject(hDc,GetStockObject  (NULL_PEN          ));
							Rectangle   (hDc,sButton.left-2,sButton.top-1,sButton.right+2,sButton.bottom+1);
						} else if (uBits&TVIS_SELECTED && uItem==pData->uSelectedItem) {
							if (pData->cHasFocus) {
								uTempColor =     GetSysColor     (COLOR_HIGHLIGHTTEXT);
								SelectObject(hDc,GetSysColorBrush(COLOR_HIGHLIGHT));
								SelectObject(hDc,GetStockObject  (NULL_PEN       ));
							} else {
								uTempColor = pData->uColors[TVC_TEXT];
								SelectObject(hDc,GetSysColorBrush(COLOR_MENU));
								SelectObject(hDc,GetStockObject  (NULL_PEN  ));
							}

							Rectangle(hDc,sButton.left-2,sButton.top-1,sButton.right+2,sButton.bottom+1);
						} else {
							if (uBits & TVIS_TRACKED)      uTempColor = pData ->uColors[TVC_TRACK];
							else if (uBits&TVIS_TEXTCOLOR) uTempColor = pExtra->uColorText;
							else						   uTempColor = pData ->uColors[TVC_TEXT ];

							sButton.right--;
							sButton.left --;
						}

						SelectObject(hDc,hPatternPen);

						if (uBits&TVIS_FOCUSED) {
							SelectObject(hDc,GetStockObject(NULL_BRUSH));
							Rectangle   (hDc,sButton.left-2,sButton.top-1,sButton.right+2,sButton.bottom+1);
						}
					}

					SetTextColor(hDc,uTempColor);
					sButton.left+=pData->iFontOff;
					DrawTextW(hDc,pText,uTextSize,&sButton,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX);

					if (uBits & (TVIS_UNTERLINE|TVIS_TRACKED)) {
						sButton.left   -= pData->iFontOff;
						sButton.right  -= pData->iFontOff+1;
						sButton.top	   += pData->iFontLine;
						sButton.bottom  = sButton.top+1;
						uOldColor=SetBkColor(hDc,uTempColor);
						ExtTextOutW(hDc,0,0,ETO_OPAQUE|ETO_CLIPPED,&sButton,NULL,0,NULL);
						SetBkColor(hDc,uOldColor);
					}

					SetTextColor(hDc,pData->uColors[TVC_TEXT]);
				} else {
					if (!pExtra->iTextPixels) {
						sButton.top		= iYpos;
						sButton.left    = sArea.left+4;
						sButton.right	= sArea.right;
						sButton.bottom	= sArea.bottom;

						if (uTextSize) {
							DrawTextW(hDc,pText,uTextSize,&sButton,DT_LEFT|DT_VCENTER|DT_SINGLELINE|DT_NOPREFIX|DT_CALCRECT);
							pExtra->iTextPixels = sButton.right-sButton.left;
						} else {
							pExtra->iTextPixels = 0;
						}
					}

					switch (pData->cColumnAlign[uExtra]) {
					case DT_CENTER:
						SetTextAlign(hDc,TA_CENTER|TA_TOP);
						ExtTextOutW(hDc,(sArea.right+sArea.left)/2,iYpos,ETO_OPAQUE|ETO_CLIPPED,&sArea,pText,uTextSize,NULL);
						SetTextAlign(hDc,TA_LEFT|TA_TOP);
						break;
					case DT_RIGHT:
						SetTextAlign(hDc,TA_RIGHT|TA_TOP);
						ExtTextOutW(hDc,sArea.right-2,iYpos,ETO_OPAQUE|ETO_CLIPPED,&sArea,pText,uTextSize,NULL);
						SetTextAlign(hDc,TA_LEFT|TA_TOP);
						break;
					default:
						ExtTextOutW(hDc,sArea.left+2,iYpos,ETO_OPAQUE|ETO_CLIPPED,&sArea,pText,uTextSize,NULL);
						break;
					}
				}
			}
		}

		sArea.top += pData->iRowHeight;
	}

	if (sArea.top<sRect.bottom) {
		sRect.top =	sArea.top;
		SelectObject(hDc,hRgnMain);
		SetBkColor  (hDc,uBkColor);
		ExtTextOutW (hDc,0,0,ETO_OPAQUE,&sRect,NULL,0,NULL);
	}

	if (pData->iMaxSizeX!=iMaxX) {
		pData->iMaxSizeX=iMaxX;
		if(pData->uColumnCount==0)
		if(pData->iMaxSizeX!=(int)pData->uOldXCount) {
			UpdateScrollX(pData);
		}
	}

	UNLOCK(pData);

 	DeleteObject(hRgnMain);

    for(uPos=0;uPos<uRgnCount;uPos++) {
        DeleteObject(hRgn[uPos]);
	}
}

WindowHandle osCreateTreeView(WindowHandle window)
{
	HWND hTreeList;

	hTreeList = CreateWindowExW(
			  WS_EX_CLIENTEDGE,
			  L"HTreeList",
			  NULL,
			  WS_CHILD | WS_VSCROLL | WS_BORDER | WS_TABSTOP | TVS_HASLINES | TVS_HASBUTTONS | TVS_LINESATROOT,
			  0,0,0,0,
			  window,
			  NULL,
			  ghModule,
			  NULL
			);
	return checkWindow(hTreeList, "HTreeList");
}

int osAddTreeViewColumn(WindowHandle treeview, PortString title, int type)
{
	TVCOLUMNW tvc;
	tvc.mask = TVCF_TEXT;
	tvc.pszText = title;
	return TreeList_InsertColumn(treeview, INT_MAX, &tvc);
}

RowHandle osAppendTreeViewItem (WindowHandle treeview, RowHandle parent)
{
	size_t i, count;

	TVINSERTSTRUCTW tvi;
	tvi.hParent = parent;
	tvi.hInsertAfter = TVI_LAST;
	tvi.item.mask = TVIF_TEXT;
	tvi.item.pszText = LPSTR_TEXTCALLBACKW;

	tvi.item.hItem = TreeList_InsertItem(treeview, &tvi);

	tvi.item.mask |= TVIF_SUBITEM;
	count = TreeList_GetColumnCount(treeview);
	for (i = 1; i < count; i++) {
		tvi.item.cChildren = i;
		TreeList_SetItem(treeview,&tvi.item);
	}
	return tvi.item.hItem;
}

void osGetTreeViewReqSize(WindowHandle treeview, int *res)
{
	res[0] = 100;
	res[1] = 100;
}
