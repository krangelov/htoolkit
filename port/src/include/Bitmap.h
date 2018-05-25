#ifndef BITMAP_H
#define BITMAP_H

#include "Types.h"

extern BitmapHandle osCreateBitmap(int width, int height);
extern void osDeleteBitmap(BitmapHandle bitmap);
extern BitmapHandle osReadBitmap (char *, int *);
extern int osWriteBitmap(BitmapHandle bitmap, char *format, char *fname);
extern int osWriteBitmapHandle(BitmapHandle bitmap, char *format, int fhandle);
extern void osSetBitmapSize (BitmapHandle bitmap, int width, int height);
extern void osGetBitmapSize (BitmapHandle bitmap, int *size);
extern CanvasHandle osGetBitmapCanvas(BitmapHandle bitmap);
extern void osReleaseBitmapCanvas(CanvasHandle canvas);
extern void *osInitEncodersEnumerator();
extern char *osGetCurrentEncoderName(CodecsEnumeratorHandle enumerator);
extern char *osGetCurrentEncoderDescription(CodecsEnumeratorHandle enumerator);
extern char *osGetCurrentEncoderMime(CodecsEnumeratorHandle enumerator);
extern BOOL osGetCurrentEncoderReadable(CodecsEnumeratorHandle enumerator);
extern BOOL osGetCurrentEncoderWritable(CodecsEnumeratorHandle enumerator);
extern char *osGetCurrentEncoderExtensions(CodecsEnumeratorHandle enumerator);
extern BOOL osSelectNextEncoder(CodecsEnumeratorHandle enumerator);
extern void osFreeEncodersEnumerator(CodecsEnumeratorHandle enumerator);

#endif
