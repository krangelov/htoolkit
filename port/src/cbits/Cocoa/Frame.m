#include "Types.h"
#include "Internals.h"
#include "Window.h"
#include "ConfigKey.h"
#include "Handlers_stub.h"

char *gAppTitle = NULL;
char *gAppName = NULL;
char *gAppVersion = NULL;
NSWindow *gFrameWindow = NULL;
NSView *gClientView = NULL;

void createMDIFrame()
{
	printf("createMDIFrame\n");
}

void createSDIFrame()
{
	/* Create Window */
	gFrameWindow =
	        [[NSWindow alloc] initWithContentRect: NSMakeRect(300, 300, 200, 100)
                                        styleMask: (NSTitledWindowMask |
                                                    NSMiniaturizableWindowMask |
                                                    NSResizableWindowMask)
                                          backing: NSBackingStoreBuffered
                                            defer: YES];
   [gFrameWindow setTitle: [NSString stringWithUTF8String: gAppTitle]];
   [gFrameWindow makeKeyAndOrderFront: [NSApplication sharedApplication].delegate];
}
