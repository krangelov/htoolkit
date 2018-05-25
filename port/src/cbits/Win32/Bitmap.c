#include "Bitmap.h"
#include "Internals.h"

typedef enum
{
    DebugEventLevelFatal,
    DebugEventLevelWarning
} DebugEventLevel;

typedef VOID (WINAPI *DebugEventProc)(DebugEventLevel level, CHAR *message);

typedef struct
{
    UINT32 GdiplusVersion;             // Must be 1
    DebugEventProc DebugEventCallback; // Ignored on free builds
    BOOL SuppressBackgroundThread;     // FALSE unless you're prepared to call
                                       // the hook/unhook functions properly
    BOOL SuppressExternalCodecs;       // FALSE unless you want GDI+ only to use
                                       // its internal image codecs.
} GdiplusStartupInput;

typedef struct {} GpBitmap;

typedef struct
{
    CLSID Clsid;
    GUID  FormatID;
    WCHAR* CodecName;
    WCHAR* DllName;
    WCHAR* FormatDescription;
    WCHAR* FilenameExtension;
    WCHAR* MimeType;
    DWORD Flags;
    DWORD Version;
    DWORD SigCount;
    DWORD SigSize;
    const BYTE* SigPattern;
    const BYTE* SigMask;
} ImageCodecInfo;

typedef struct
{
    GUID    Guid;               // GUID of the parameter
    ULONG   NumberOfValues;     // Number of the parameter values
    ULONG   Type;               // Value type, like ValueTypeLONG  etc.
    VOID*   Value;              // A pointer to the parameter values
} EncoderParameter;

typedef struct
{
    UINT Count;                      // Number of parameters in this structure
    EncoderParameter Parameter[1];   // Parameter values
} EncoderParameters;


typedef int  (WINAPI *GdiplusStartupProc)  (ULONG_PTR* token, const GdiplusStartupInput* input, void * output);
typedef void (WINAPI *GdiplusShutdownProc) (ULONG_PTR  token);
typedef int  (WINAPI *GdipCreateBitmapFromHBITMAPProc) (HBITMAP hBitmap, HPALETTE hPalette, GpBitmap **ppGpBitmap);
typedef int  (WINAPI *GdipGetImageEncodersSizeProc) (UINT *numEncoders, UINT *size);
typedef int  (WINAPI *GdipGetImageEncodersProc) (UINT numEncoders, UINT size, ImageCodecInfo *encoders);
typedef int  (WINAPI *GdipSaveImageToFileProc) (GpBitmap *gpBitmap, WCHAR*filename, CLSID *clsidEncoder, EncoderParameters *encoderParams);
typedef void (WINAPI *GdipDisposeImageProc) (GpBitmap *gpBitmap);
typedef int  (WINAPI *GdipCreateBitmapFromFileICMProc) (WCHAR *fname, GpBitmap **ppGpBitmap);
typedef int  (WINAPI *GdipCreateHBITMAPFromBitmapProc) (GpBitmap *gpBitmap, HBITMAP *phBitmap, DWORD Argb);

static HMODULE		hGdiPlusDll = NULL;
static ULONG_PTR	gdiplusToken;

// exported GdiPlus functions
static GdiplusStartupProc  GdiplusStartup;
static GdiplusShutdownProc GdiplusShutdown;
static GdipCreateBitmapFromHBITMAPProc GdipCreateBitmapFromHBITMAP;
static GdipGetImageEncodersSizeProc GdipGetImageEncodersSize;
static GdipGetImageEncodersProc GdipGetImageEncoders;
static GdipSaveImageToFileProc GdipSaveImageToFile;
static GdipDisposeImageProc GdipDisposeImage;
static GdipCreateBitmapFromFileICMProc GdipCreateBitmapFromFileICM;
static GdipCreateHBITMAPFromBitmapProc GdipCreateHBITMAPFromBitmap;

static BOOL bGdiPlusInitialized = FALSE;

static BOOL initGdiPlus()
{
	if (!bGdiPlusInitialized)
	{
		GdiplusStartupInput gdiplusStartupInput;
		gdiplusStartupInput.GdiplusVersion = 1;
		gdiplusStartupInput.DebugEventCallback = NULL;
		gdiplusStartupInput.SuppressBackgroundThread = FALSE;
		gdiplusStartupInput.SuppressExternalCodecs = FALSE;

		bGdiPlusInitialized = TRUE;

		hGdiPlusDll = LoadLibrary("GdiPlus.dll");
		if (!hGdiPlusDll) return FALSE;

		GdiplusStartup  = (GdiplusStartupProc)  GetProcAddress(hGdiPlusDll, "GdiplusStartup");
		GdiplusShutdown = (GdiplusShutdownProc) GetProcAddress(hGdiPlusDll, "GdiplusShutdown");
		GdipCreateBitmapFromHBITMAP = (GdipCreateBitmapFromHBITMAPProc) GetProcAddress(hGdiPlusDll, "GdipCreateBitmapFromHBITMAP");
		GdipGetImageEncodersSize = (GdipGetImageEncodersSizeProc) GetProcAddress(hGdiPlusDll, "GdipGetImageEncodersSize");
		GdipGetImageEncoders = (GdipGetImageEncodersProc) GetProcAddress(hGdiPlusDll, "GdipGetImageEncoders");
		GdipSaveImageToFile = (GdipSaveImageToFileProc) GetProcAddress(hGdiPlusDll, "GdipSaveImageToFile");
		GdipDisposeImage = (GdipDisposeImageProc) GetProcAddress(hGdiPlusDll, "GdipDisposeImage");
		GdipCreateBitmapFromFileICM = (GdipCreateBitmapFromFileICMProc) GetProcAddress(hGdiPlusDll, "GdipCreateBitmapFromFileICM");
		GdipCreateHBITMAPFromBitmap = (GdipCreateHBITMAPFromBitmapProc) GetProcAddress(hGdiPlusDll, "GdipCreateHBITMAPFromBitmap");

		if (!GdiplusStartup ||
			!GdiplusShutdown ||
			!GdipCreateBitmapFromHBITMAP ||
			!GdipGetImageEncodersSize ||
			!GdipGetImageEncoders ||
			!GdipSaveImageToFile ||
			!GdipDisposeImage ||
			!GdipCreateBitmapFromFileICM ||
			!GdipCreateHBITMAPFromBitmap)
		{
			FreeLibrary(hGdiPlusDll);
			hGdiPlusDll = NULL;
			return FALSE;
		}

		GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
	}

	return (hGdiPlusDll != NULL);
}

void doneGdiPlus()
{
	if (bGdiPlusInitialized)
	{
		bGdiPlusInitialized = FALSE;
		if (hGdiPlusDll)
		{
			GdiplusShutdown(gdiplusToken);

			FreeLibrary(hGdiPlusDll);
			hGdiPlusDll = NULL;
		}
	}
}

BitmapHandle osReadBitmap(char *fname, int *pRes)
{
	BitmapHandle hBitmap;
	HBITMAP hbmp;
	BITMAPINFO bi;
	GpBitmap *gpBitmap;
	HDC hdc;

	*pRes    = 1;

	if (initGdiPlus())
	{
		WCHAR *lpwBuffer;
		int nFNameLen;

		nFNameLen = strlen(fname);
		lpwBuffer = (WCHAR *) rmalloc((nFNameLen+1)*2);

		if (!MultiByteToWideChar(CP_OEMCP, 0, fname, nFNameLen, lpwBuffer, nFNameLen))
		{
			rfree(lpwBuffer);
			return NULL;
		}
		lpwBuffer[nFNameLen] = 0;

		gpBitmap = NULL;
		if (GdipCreateBitmapFromFileICM(lpwBuffer, &gpBitmap) != 0 || !gpBitmap)
		{
			switch (GetLastError())
			{
			case 0:
				*pRes  = 2;
				 break;
			case ERROR_FILE_NOT_FOUND:
			case ERROR_PATH_NOT_FOUND:
				 *pRes = 3;
				 break;
			case ERROR_ACCESS_DENIED:
				*pRes  = 4;
				break;
			}

			rfree(lpwBuffer);
			return NULL;
		}

		rfree(lpwBuffer);

		hbmp = NULL;
		if (GdipCreateHBITMAPFromBitmap(gpBitmap, &hbmp, 0xFF000000) != 0)
		{
			GdipDisposeImage(gpBitmap);
			return NULL;
		}

		GdipDisposeImage(gpBitmap);

		if (!hbmp)
			return NULL;
	}
	else
	{
		hbmp = (HBITMAP) LoadImage(ghModule, fname, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);

		if (!hbmp)
		{
			switch (GetLastError())
			{
			case ERROR_FILE_NOT_FOUND:
			case ERROR_PATH_NOT_FOUND:
				*pRes = 3;
				return NULL;
			case ERROR_ACCESS_DENIED:
				*pRes  = 4;
				break;
			default:
				*pRes = 2;
				return NULL;
			}
		}
	}

	memset(&bi, 0, sizeof(bi));
	bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);

	hdc = CreateDC ("DISPLAY",NULL,NULL,NULL);
	if (!GetDIBits(hdc, hbmp, 0, 0, NULL, &bi, DIB_RGB_COLORS))
	{
		DeleteObject(hbmp);
		DeleteDC(hdc);
		return NULL;
	}
	DeleteDC(hdc);

	hBitmap = rmalloc(sizeof(*hBitmap));
	hBitmap->hBitmap = hbmp;
	hBitmap->sourcesize.cx = bi.bmiHeader.biWidth;
	hBitmap->sourcesize.cy = bi.bmiHeader.biHeight;
	hBitmap->destsize.cx = bi.bmiHeader.biWidth;
	hBitmap->destsize.cy = bi.bmiHeader.biHeight;

	*pRes = 0;
	return hBitmap;
}	/* osCreateBitmap */

BitmapHandle osCreateBitmap(int width, int height)
{
	HDC hScreenDC;
	BitmapHandle bitmap;

	hScreenDC = CreateDC("DISPLAY",NULL,NULL,NULL);
	if (!hScreenDC)
		return NULL;

	bitmap = rmalloc(sizeof(*bitmap));
	bitmap->hBitmap = CreateCompatibleBitmap(hScreenDC, width, height);
	bitmap->sourcesize.cx = width;
	bitmap->sourcesize.cy = height;
	bitmap->destsize.cx = width;
	bitmap->destsize.cy = height;

	DeleteDC(hScreenDC);

	return bitmap;
}

void osSetBitmapSize (BitmapHandle bitmap, int width, int height)
{
	bitmap->destsize.cx = width;
	bitmap->destsize.cy = height;
}

void osGetBitmapSize (BitmapHandle bitmap, int *size)
{
	size[0] = bitmap->destsize.cx;
	size[1] = bitmap->destsize.cy;
}

void osDeleteBitmap (BitmapHandle bitmap)
{
	if (!DeleteObject (bitmap->hBitmap))
	{
		printf("Fatal error in WinDisposeBitmap: DeleteObject returned FALSE.\n");
		exit(1);
	}

	free(bitmap);
}

CanvasHandle osGetBitmapCanvas(BitmapHandle bitmap)
{
	HDC hScreenDC;
	CanvasHandle canvas;

	canvas = (CanvasHandle) rmalloc(sizeof(*canvas));
	memset(canvas, 0, sizeof(*canvas));
	canvas->bInvalidated = FALSE;

	hScreenDC = CreateDC("DISPLAY",NULL,NULL,NULL);
	if (!hScreenDC)
		return NULL;

	canvas->hDC = CreateCompatibleDC(hScreenDC);

	if (bitmap->destsize.cx != bitmap->sourcesize.cx || bitmap->destsize.cy != bitmap->sourcesize.cy)
	{
		HDC hSourceDC;
		HBITMAP hBitmap;

		hBitmap = CreateCompatibleBitmap(hScreenDC, bitmap->destsize.cx, bitmap->destsize.cy);
		SelectObject(canvas->hDC, hBitmap);

		hSourceDC = CreateCompatibleDC(hScreenDC);
		SelectObject(hSourceDC, bitmap->hBitmap);

		StretchBlt(canvas->hDC, 0, 0, bitmap->destsize.cx, bitmap->destsize.cy, hSourceDC, 0, 0, bitmap->sourcesize.cx, bitmap->sourcesize.cy, SRCCOPY);

		DeleteDC(hSourceDC);

		DeleteObject(bitmap->hBitmap);
		bitmap->hBitmap = hBitmap;
		bitmap->sourcesize = bitmap->destsize;
	}
	else
		SelectObject(canvas->hDC, bitmap->hBitmap);

	DeleteDC(hScreenDC);

	return canvas;
}

void osReleaseBitmapCanvas(CanvasHandle canvas)
{
	DeleteDC(canvas->hDC);
	rfree(canvas);
}

static int GetEncoderClsid(const WCHAR* format, CLSID* pClsid)
{
	UINT  j;
	UINT  num = 0;          // number of image encoders
	UINT  size = 0;         // size of the image encoder array in bytes

	ImageCodecInfo* pImageCodecInfo = NULL;

	GdipGetImageEncodersSize(&num, &size);
	if(size == 0)
		return -1;  // Failure

	pImageCodecInfo = (ImageCodecInfo*) rmalloc(size);
	if(pImageCodecInfo == NULL)
		return -1;  // Failure

	GdipGetImageEncoders(num, size, pImageCodecInfo);

	for (j = 0; j < num; ++j)
	{
		if (wcscmp(pImageCodecInfo[j].MimeType, format) == 0 )
		{
			*pClsid = pImageCodecInfo[j].Clsid;
			free(pImageCodecInfo);
			return j;  // Success
		}
	}

	rfree(pImageCodecInfo);
	return -1;  // Failure
}

int osWriteBitmap(BitmapHandle bitmap, char *format, char *fname)
{
	HDC hScreenDC;
	HBITMAP hBitmap;
	BOOL bFlag;

	hScreenDC = CreateDC("DISPLAY",NULL,NULL,NULL);
	if (!hScreenDC)
		return 1;

	if (bitmap->destsize.cx != bitmap->sourcesize.cx || bitmap->destsize.cy != bitmap->sourcesize.cy)
	{
		HDC hSourceDC, hDestDC;

		hDestDC = CreateCompatibleDC(hScreenDC);
		hBitmap = CreateCompatibleBitmap(hScreenDC, bitmap->destsize.cx, bitmap->destsize.cy);
		SelectObject(hDestDC, hBitmap);

		hSourceDC = CreateCompatibleDC(hScreenDC);
		SelectObject(hSourceDC, bitmap->hBitmap);

		StretchBlt(hDestDC, 0, 0, bitmap->destsize.cx, bitmap->destsize.cy, hSourceDC, 0, 0, bitmap->sourcesize.cx, bitmap->sourcesize.cy, SRCCOPY);

		DeleteDC(hDestDC);
		DeleteDC(hSourceDC);

		bFlag = TRUE;
	}
	else
	{
		hBitmap = bitmap->hBitmap;
		bFlag = FALSE;
	}

	if (initGdiPlus())
	{
		CLSID cls;
		HPALETTE hPal;
		GpBitmap *gpBitmap;
		WCHAR *lpwBuffer;
		int nFormatLen, nFNameLen;

		hPal = (HPALETTE) GetCurrentObject(hScreenDC, OBJ_PAL);

		nFormatLen = strlen(format);
		nFNameLen  = strlen(fname);
		lpwBuffer = (WCHAR *) rmalloc((max(nFormatLen,nFNameLen)+1)*2);

		if (!MultiByteToWideChar(CP_OEMCP, 0, format, nFormatLen, lpwBuffer, nFormatLen))
		{
			rfree(lpwBuffer);
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 1;
		}
		lpwBuffer[nFormatLen] = 0;

		if (GetEncoderClsid(lpwBuffer, &cls) < 0)
		{
			rfree(lpwBuffer);
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 2;
		}

		if (!MultiByteToWideChar(CP_OEMCP, 0, fname, nFNameLen, lpwBuffer, nFNameLen))
		{
			rfree(lpwBuffer);
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 1;
		}
		lpwBuffer[nFNameLen] = 0;

		gpBitmap = NULL;
		if (GdipCreateBitmapFromHBITMAP(hBitmap, hPal, &gpBitmap) != 0 || !gpBitmap)
		{
			rfree(lpwBuffer);
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 1;
		}

		if (GdipSaveImageToFile(gpBitmap, lpwBuffer, &cls, NULL) != 0)
		{
			rfree(lpwBuffer);
			GdipDisposeImage(gpBitmap);
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 3;
		}

		rfree(lpwBuffer);
		GdipDisposeImage(gpBitmap);
	}
	else
	{
		HANDLE hFile;
		LPBITMAPFILEHEADER pBFH;
		LPBITMAPINFO pBMI;
		PBYTE       pBits;
		BITMAPINFO  bmi;
		ULONG sizBMI;
		char *pBuffer;
		DWORD dw;

		if (strcmp(format,"image/bmp") != 0)
		{
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 2;
		}

		// Let the graphics engine to retrieve the dimension of the bitmap for us
		// GetDIBits uses the size to determine if its BITMAPCOREINFO or BITMAPINFO
		// if BitCount != 0, color table will be retrieved
		bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
		bmi.bmiHeader.biBitCount = 0;             // dont get the color table
		if (!GetDIBits(hScreenDC, hBitmap, 0, 0, NULL, &bmi, DIB_RGB_COLORS))
		{
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 1;
		}

		switch (bmi.bmiHeader.biBitCount)
		{
			case 24:                                      // has color table
				sizBMI = sizeof(BITMAPINFOHEADER);
				break;
			case 16:
			case 32:
				sizBMI = sizeof(BITMAPINFOHEADER)+sizeof(DWORD)*3;
				break;
			default:
				sizBMI = sizeof(BITMAPINFOHEADER)+sizeof(RGBQUAD)*(1<<bmi.bmiHeader.biBitCount);
				break;
		}

		dw = sizeof(BITMAPFILEHEADER)+sizBMI+bmi.bmiHeader.biSizeImage;

		pBuffer = rmalloc(dw);

		pBFH  = (LPBITMAPFILEHEADER) pBuffer;
		pBMI  = (LPBITMAPINFO) (pBuffer+sizeof(BITMAPFILEHEADER));
		pBits = (PBYTE) (pBuffer+sizeof(BITMAPFILEHEADER)+sizBMI);

		// But first, fill in the info for the BitmapFileHeader
		pBFH->bfType = 0x4D42;                            // BM
		pBFH->bfSize = dw;
		pBFH->bfReserved1 = 0;
		pBFH->bfReserved2 = 0;
		pBFH->bfOffBits = sizeof(BITMAPFILEHEADER)+sizBMI;

		memcpy(pBMI,&bmi,sizeof(BITMAPINFOHEADER));

		// Bitmap cant be selected into a DC when calling GetDIBits
		// Assume that the hDC is the DC where the bitmap would have been selected
		// if indeed it has been selected
		if (!GetDIBits(hScreenDC, hBitmap, 0, pBMI->bmiHeader.biHeight, pBits, pBMI, DIB_RGB_COLORS))
		{
			rfree(pBuffer);
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 1;
		}

		if ((hFile = CreateFile(fname,GENERIC_WRITE,0,NULL,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,NULL)) == INVALID_HANDLE_VALUE)
		{
			rfree(pBuffer);
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 3;
		}

		// Write out the file header now
		if (!WriteFile(hFile, pBuffer, pBFH->bfSize, &dw, NULL) || dw != pBFH->bfSize)
		{
			CloseHandle(hFile);
			rfree(pBuffer);
			DeleteDC(hScreenDC);
			if (bFlag) DeleteObject(hBitmap);
			return 3;
		}

		CloseHandle(hFile);
		rfree(pBuffer);
	}

	DeleteDC(hScreenDC);
	if (bFlag) DeleteObject(hBitmap);

	return 0;
}

CodecsEnumeratorHandle osInitEncodersEnumerator()
{
	UINT  num = 0;          // number of image encoders
	UINT  size = 0;         // size of the image encoder array in bytes
	char *pEnumerator = NULL;
	ImageCodecInfo *pImageCodecInfo = NULL;

	if (initGdiPlus())
	{
		GdipGetImageEncodersSize(&num, &size);
		if(size == 0)
			return NULL;  // Failure

		pEnumerator = (char*) rmalloc(size+sizeof(UINT)*2);
		if (pEnumerator == NULL)
			return NULL;  // Failure

		pImageCodecInfo = (ImageCodecInfo*) (pEnumerator+sizeof(UINT)*2);
		GdipGetImageEncoders(num, size, pImageCodecInfo);
	}
	else
	{
		num = 1;
		pEnumerator = (char*) rmalloc(sizeof(ImageCodecInfo)+sizeof(UINT)*2);
		if (pEnumerator == NULL)
			return NULL;  // Failure

		pImageCodecInfo = (ImageCodecInfo*) (pEnumerator+sizeof(UINT)*2);
		pImageCodecInfo->CodecName = L"HToolkit BMP Codec";
		pImageCodecInfo->FormatDescription = L"BMP";
		pImageCodecInfo->FilenameExtension = L"*.BMP";
		pImageCodecInfo->MimeType = L"image/bmp";
   	}

	((UINT *) pEnumerator)[0] = -1;
	((UINT *) pEnumerator)[1] = num;
	return (CodecsEnumeratorHandle) pEnumerator;
}

char *osGetCurrentEncoderName(CodecsEnumeratorHandle enumerator)
{
	UINT nIndex = ((UINT *) enumerator)[0];
	UINT nCount = ((UINT *) enumerator)[1];
	ImageCodecInfo *pImageCodecInfo = (ImageCodecInfo*) (((char *) enumerator)+sizeof(UINT)*2);

	if (nIndex < nCount)
	{
		int nChars = wcslen(pImageCodecInfo[nIndex].CodecName);
		char *buffer = (char *) rmalloc((nChars+1)*2);

		nChars = WideCharToMultiByte(CP_OEMCP, 0,
				pImageCodecInfo[nIndex].CodecName,
				nChars, buffer,
				nChars*2, NULL, NULL);

		if (!nChars)
		{
			rfree(buffer);
			return NULL;
		}

		buffer[nChars] = 0;
		return buffer;
	}

	return NULL;
}

char *osGetCurrentEncoderDescription(CodecsEnumeratorHandle enumerator)
{
	UINT nIndex = ((UINT *) enumerator)[0];
	UINT nCount = ((UINT *) enumerator)[1];
	ImageCodecInfo *pImageCodecInfo = (ImageCodecInfo*) (((char *) enumerator)+sizeof(UINT)*2);

	if (nIndex < nCount)
	{
		int nChars = wcslen(pImageCodecInfo[nIndex].FormatDescription);
		char *buffer = (char *) rmalloc((nChars+1)*2);

		nChars = WideCharToMultiByte(CP_OEMCP, 0,
				pImageCodecInfo[nIndex].FormatDescription,
				nChars, buffer,
				nChars*2, NULL, NULL);

		if (!nChars)
		{
			rfree(buffer);
			return NULL;
		}

		buffer[nChars] = 0;
		return buffer;
	}

	return NULL;
}

char *osGetCurrentEncoderMime(CodecsEnumeratorHandle enumerator)
{
	UINT nIndex = ((UINT *) enumerator)[0];
	UINT nCount = ((UINT *) enumerator)[1];
	ImageCodecInfo *pImageCodecInfo = (ImageCodecInfo*) (((char *) enumerator)+sizeof(UINT)*2);

	if (nIndex < nCount)
	{
		int nChars = wcslen(pImageCodecInfo[nIndex].MimeType);
		char *buffer = (char *) rmalloc((nChars+1)*2);

		nChars = WideCharToMultiByte(CP_OEMCP, 0,
				pImageCodecInfo[nIndex].MimeType,
				nChars, buffer,
				nChars*2, NULL, NULL);

		if (!nChars)
		{
			rfree(buffer);
			return NULL;
		}

		buffer[nChars] = 0;
		return buffer;
	}

	return NULL;
}

BOOL osGetCurrentEncoderReadable(CodecsEnumeratorHandle enumerator)
{
	return TRUE;
}

BOOL osGetCurrentEncoderWritable(CodecsEnumeratorHandle enumerator)
{
	return TRUE;
}

char *osGetCurrentEncoderExtensions(CodecsEnumeratorHandle enumerator)
{
	UINT nIndex = ((UINT *) enumerator)[0];
	UINT nCount = ((UINT *) enumerator)[1];
	ImageCodecInfo *pImageCodecInfo = (ImageCodecInfo*) (((char *) enumerator)+sizeof(UINT)*2);

	if (nIndex < nCount)
	{
		int nChars = wcslen(pImageCodecInfo[nIndex].FilenameExtension);
		char *ps, *pd, *buffer = (char *) rmalloc((nChars+2)*2);

		nChars = WideCharToMultiByte(CP_OEMCP, 0,
				pImageCodecInfo[nIndex].FilenameExtension,
				nChars, buffer,
				nChars*2, NULL, NULL);

		if (!nChars)
		{
			rfree(buffer);
			return NULL;
		}

		buffer[nChars] = 0;

		ps = buffer;
		pd = buffer;

		ps += 2;
		while (*ps)
		{
			if (*ps == ';')
			{
				ps += 3;
				*pd++ = 0;
			}
			else
				*pd++ = tolower(*ps++);
		}
		*pd++ = 0;
		*pd++ = 0;

		return buffer;
	}

	return NULL;
}

BOOL osSelectNextEncoder(CodecsEnumeratorHandle enumerator)
{
	UINT nIndex, nCount;

	((UINT *) enumerator)[0]++;

	nIndex = ((UINT *) enumerator)[0];
	nCount = ((UINT *) enumerator)[1];

	return (nIndex < nCount);
}

void osFreeEncodersEnumerator(CodecsEnumeratorHandle enumerator)
{
	rfree(enumerator);
}
