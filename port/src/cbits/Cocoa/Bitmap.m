#include "Bitmap.h"
#include "Internals.h"

BitmapHandle osReadBitmap(char *filename, int *pRes)
{
	NSImage *img = [[NSImage new] initByReferencingFile: [NSString stringWithUTF8String: filename]];
	if (img.valid) {
		*pRes = 0;
		return img;
	}

	*pRes = 1;
	[img release];
	return NULL;
}

BitmapHandle osCreateBitmap(int width, int height)
{
	printf("osCreateBitmap\n");
	return NULL;
}

void osSetBitmapSize (BitmapHandle bitmap, int width, int height)
{
	printf("osSetBitmapSize\n");
}

void osGetBitmapSize (BitmapHandle bitmap, int *size)
{
	NSSize nsSize = bitmap.size;
	size[0] = nsSize.width;
	size[1] = nsSize.height;
}

void osDeleteBitmap (BitmapHandle bitmap)
{
	[bitmap release];
}

CanvasHandle osGetBitmapCanvas(BitmapHandle bitmap)
{
	printf("osGetBitmapCanvas\n");
	return NULL;
}

void osReleaseBitmapCanvas(CanvasHandle canvas)
{
	printf("osReleaseBitmapCanvas\n");
}

int osWriteBitmap(BitmapHandle bitmap, char *format, char *fname)
{
	printf("osWriteBitmap\n");
	return 0;
}

struct CodecsEnumerator {
	NSArray* utis;
	size_t index;
	NSDictionary* descr;
};

void *osInitEncodersEnumerator()
{
	CodecsEnumeratorHandle enumerator = malloc(sizeof(struct CodecsEnumerator));
	enumerator->utis  = NSImage.imageTypes;
	enumerator->index = -1;
	enumerator->descr = NULL;
	return enumerator;
}

BOOL osSelectNextEncoder(CodecsEnumeratorHandle enumerator)
{
	enumerator->index++;
	if (enumerator->index >= [enumerator->utis count]) {
		enumerator->descr = NULL;
		return FALSE;
	}

	enumerator->descr = (NSDictionary*) UTTypeCopyDeclaration((CFStringRef) [enumerator->utis objectAtIndex: enumerator->index]);
	return TRUE;
}

char *osGetCurrentEncoderName(CodecsEnumeratorHandle enumerator)
{
	NSString* name = (NSString*) [enumerator->descr objectForKey: (NSString*) kUTTypeIdentifierKey];
	return (name == NULL) ? NULL : strdup([name UTF8String]);
};

char *osGetCurrentEncoderDescription(CodecsEnumeratorHandle enumerator)
{
	NSString* descr = (NSString*) [enumerator->descr objectForKey: (NSString*) kUTTypeDescriptionKey];
	return (descr == NULL) ? NULL : strdup([descr UTF8String]);
};

char *osGetCurrentEncoderMime(CodecsEnumeratorHandle enumerator)
{
	NSDictionary* tags = (NSDictionary*) [enumerator->descr objectForKey: (NSString*) kUTTypeTagSpecificationKey];
	NSObject* obj = [tags objectForKey: (NSString*) kUTTagClassMIMEType];
	if ([obj isKindOfClass:[NSString class]]) {
		return strdup([(NSString*)obj UTF8String]);
	} else if ([obj isKindOfClass:[NSArray class]]) {
		obj = [(NSArray*)obj objectAtIndex: 0];
		return strdup([(NSString*)obj UTF8String]);
	} else {
		return NULL;
	}
};

BOOL osGetCurrentEncoderReadable(CodecsEnumeratorHandle enumerator)
{
	return TRUE;
};

BOOL osGetCurrentEncoderWritable(CodecsEnumeratorHandle enumerator)
{
	return TRUE;
};

char *osGetCurrentEncoderExtensions(CodecsEnumeratorHandle enumerator)
{
	NSDictionary* tags = (NSDictionary*) [enumerator->descr objectForKey: (NSString*) kUTTypeTagSpecificationKey];
	NSObject* obj = [tags objectForKey: (NSString*) kUTTagClassFilenameExtension];
	
	if ([obj isKindOfClass:[NSString class]]) {
		const char* c_ext = [(NSString*)obj UTF8String];
		size_t len = strlen(c_ext);
		char* buffer = malloc(len+2);
		strcpy(buffer,c_ext);
		buffer[len+1] = 0;
		return buffer;
	} else {
		NSArray* exts = (NSArray*) obj;

		size_t n_count = [exts count];
		const char** c_exts = alloca(n_count*sizeof(char*));

		size_t len = 0;	
		for (size_t i = 0; i < n_count; i++) {
			c_exts[i] = [[exts objectAtIndex: i] UTF8String];
			len += strlen(c_exts[i])+1;
		}
		len++;

		char* buffer = malloc(len);
		char* s = buffer;
		for (size_t i = 0; i < n_count; i++) {
			strcpy(s,c_exts[i]);
			s += strlen(c_exts[i])+1;
		}
		*s = 0;
		return buffer;
	}
};

void osFreeEncodersEnumerator(CodecsEnumeratorHandle enumerator)
{
	free(enumerator);
}
