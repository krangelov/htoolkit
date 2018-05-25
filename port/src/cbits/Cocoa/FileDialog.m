#include "CommonDialogs.h"
#include "Internals.h"

static void SetFileChooserFilter(NSSavePanel *panel, char *filter)
{
	NSMutableArray *arr = [NSMutableArray new];

	for (;;)
	{
		char *name, *exts, *ext, *s;

		name = filter;
		exts = name + strlen(name)+1;
		filter = exts + strlen(exts)+1;

		if (*name == 0 || *exts == 0)
			break;

		s = exts;
		for (;;)
		{
			ext = s;
			if (ext[0]=='*' && ext[1]=='.')
				ext += 2;

			while (*s != ';' && *s != 0)
				s++;

			if (*s)
			{
				*(s++) = 0;
				[arr addObject: [NSString stringWithUTF8String: ext]];
			}
			else
			{
				[arr addObject: [NSString stringWithUTF8String: ext]];
				break;
			}
		}
	}

	panel.allowedFileTypes = arr;
}

static char* RunFileChooser(NSSavePanel *panel)
{
	NSModalResponse result = [panel runModal];
	if (result == NSFileHandlingPanelOKButton) {
		NSURL* url = [panel URL];
		if (url.fileURL) {
			return strdup([url fileSystemRepresentation]);
		}
	}

	return NULL;
}

static char* RunFilesChooser(NSOpenPanel *panel)
{
	char* buffer = NULL;

	NSModalResponse result = [panel runModal];
	if (result == NSFileHandlingPanelOKButton) {
		NSArray *urls = [panel URLs];
		NSUInteger n_urls = [urls count];
		size_t len   = 0;
		size_t count = 0;
		const char** paths = alloca(n_urls*sizeof(char*));
		for (NSUInteger i = 0; i < n_urls; i++) {
			NSURL* url = [urls objectAtIndex: i];
			if (url.fileURL) {
				const char* path = [url fileSystemRepresentation];
				paths[count++] = path;
				len += strlen(path)+1;
			}
		}
		len++;

		if (count > 0) {
			buffer = malloc(len);
			char* p = buffer;
			for (size_t i = 0; i < count; i++) {
				strcpy(p,paths[i]);
				p += strlen(paths[i])+1;
			}
			*p = 0;
		}
	}

	return buffer;
}

char *osSelectDirectory(char *title, WindowHandle owner)
{
	NSOpenPanel *panel = [NSOpenPanel new];
	panel.canChooseFiles = NO;
	panel.canChooseDirectories = YES;
	[panel setMessage: [NSString stringWithUTF8String: title]];
	return RunFileChooser(panel);
}

char *osSelectInputFile(char *title, char *filter, WindowHandle owner)
{
	NSOpenPanel *panel = [NSOpenPanel new];
	[panel setMessage: [NSString stringWithUTF8String: title]];
	SetFileChooserFilter(panel, filter);
	return RunFileChooser(panel);
}

char *osSelectInputFiles(char *title, char *filter, WindowHandle owner)
{
	NSOpenPanel *panel = [NSOpenPanel new];
	[panel setAllowsMultipleSelection:YES];
	[panel setMessage: [NSString stringWithUTF8String: title]];
	SetFileChooserFilter(panel, filter);
	return RunFilesChooser(panel);
}

char *osSelectOutputFile(char *title, char *filter, char *nameptr, WindowHandle owner)
{
	NSSavePanel *panel = [NSSavePanel new];
	[panel setMessage: [NSString stringWithUTF8String: title]];
	SetFileChooserFilter(panel, filter);
	return RunFileChooser(panel);
}
