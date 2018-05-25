#include "Window.h"
#include "Internals.h"
#include "Handlers_stub.h"

@interface PortView : NSView
{
}

@end

@implementation PortView

- (BOOL) isFlipped
{
	return YES;
}

@end

void osInvalidateWindow(WindowHandle window)
{
	printf("osInvalidateWindow\n");
};

void osInvalidateWindowRect(WindowHandle window, int left, int top, int right, int bottom)
{
	printf("osInvalidateWindowRect\n");
}

WindowHandle osCreateWindow()
{
	if (gDocumentInterface == 1) {
		if (gMainWindow != NULL) {
			return NULL;
		}
	}

	NSWindow* window =
		[[NSWindow alloc] initWithContentRect: NSMakeRect(300, 300, 200, 100)
		                            styleMask: (NSTitledWindowMask |
		                                        NSMiniaturizableWindowMask |
		                                        NSResizableWindowMask)
		                              backing: NSBackingStoreBuffered
		                                defer: YES];
	[window setTitle: [NSString stringWithUTF8String: gAppTitle]];
	[window makeKeyAndOrderFront: [NSApplication sharedApplication].delegate];
	
	PortView* view = [[PortView alloc] init];
	[window setContentView: view];

	return view;
}

WindowHandle osCreateDialog(WindowHandle parent)
{
	printf("osCreateDialog\n");
	return NULL;
}

WindowHandle osCreateCompoundControl(WindowHandle form)
{
	printf("osCreateCompoundControl\n");
	return NULL;
}

void osGetCompoundControlReqSize(WindowHandle listbox, int *res)
{
	printf("osGetCompoundControlReqSize\n");
}

void osSetWindowColor(WindowHandle window, int foreColor, int backColor, int hatchStyle, BitmapHandle hatchBitmap)
{
	printf("osSetWindowColor\n");
}

char *osGetWindowTitle(WindowHandle window)
{
	return strdup([window.window.title UTF8String]);
};

void osSetWindowTitle(WindowHandle window, char *title)
{
	window.window.title = [NSString stringWithUTF8String: title];
};

void osGetWindowViewSize(WindowHandle window, int *res)
{
	printf("osGetWindowViewSize\n");
}

void osSetWindowViewSize(WindowHandle window, int w, int h)
{
	printf("osSetWindowViewSize\n");
}

void osSetWindowDomainSize(WindowHandle window, int cx, int cy)
{
	printf("osSetWindowDomainSize\n");
}

void osSetWindowScrollOrigin(WindowHandle window, int x, int y)
{
	printf("osSetWindowScrollOrigin\n");
}

void osGetWindowScrollOrigin(WindowHandle window, int *res)
{
	printf("osGetWindowScrollOrigin\n");
}

void osSetWindowLineSize(WindowHandle window, int cx, int cy)
{
	printf("osSetWindowLineSize\n");
}

void osGetWindowLineSize(WindowHandle window, int *res)
{
	printf("osGetWindowLineSize\n");
}

void osSetWindowPageSize(WindowHandle window, int cx, int cy)
{
	printf("osSetWindowPageSize\n");
}

void osGetWindowPageSize(WindowHandle window, int *res)
{
	printf("osGetWindowPageSize\n");
}

void osSetWindowVisible(WindowHandle window, BOOL visible)
{
	printf("osSetWindowVisible\n");
};

BOOL osGetWindowVisible(WindowHandle window)
{
	printf("osGetWindowVisible\n");
	return 0;
};

void osRunDialog(WindowHandle window)
{
	printf("osRunDialog\n");
}

BOOL osDismissWindow(WindowHandle window)
{
	printf("osDismissWindow\n");
	return 0;
}

void osDestroyWindow(WindowHandle window)
{
	printf("osDestroyWindow\n");
}

void osSetWindowEnabled(WindowHandle window, BOOL enabled)
{
	printf("osSetWindowEnabled\n");
}

BOOL osGetWindowEnabled(WindowHandle window)
{
	printf("osGetWindowEnabled\n");
	return 0;
}

CanvasHandle osGetWindowCanvas(WindowHandle window)
{
	printf("osGetWindowCanvas\n");
	return NULL;
}	/* osGetWindowCanvas */

void osReleaseWindowCanvas(WindowHandle widget, CanvasHandle canvas)
{
	printf("osReleaseWindowCanvas\n");
}	/* osReleaseWindowCanvas */

void osMoveResizeControl(WindowHandle ctrl, int x, int y, int w, int h)
{
	printf("(%d,%d,%d,%d)\n", x,y,w,h);
	ctrl.frame = NSMakeRect(x, y, w, h);
}

void osGetControlRect(WindowHandle ctrl, int *res)
{
	NSRect rect = ctrl.frame;
	res[0] = rect.origin.x;
	res[1] = rect.origin.y;
	res[2] = rect.size.width;
	res[3] = rect.size.height;
}

void osSetControlEnabled(WindowHandle ctrl, BOOL enabled)
{
	((NSControl*) ctrl).enabled = enabled;
}

BOOL osGetControlEnabled(WindowHandle ctrl)
{
	return ((NSControl*) ctrl).enabled;
}

void osSetControlVisible(WindowHandle ctrl, BOOL visible)
{
	printf("osSetControlVisible\n");
}

BOOL osGetControlVisible(WindowHandle ctrl)
{
	printf("osGetControlVisible\n");
	return 0;
}

void osSetControlTip(WindowHandle ctrl, char *text)
{
	printf("osSetControlTip\n");
}

char *osGetControlTip(WindowHandle ctrl)
{
	printf("osGetControlTip\n");
	return NULL;
}

void osSetWindowPosition(WindowHandle window, int position, int x0, int y0, int x1, int y1)
{
	printf("osSetWindowPosition\n");
}

void osGetWindowRect(WindowHandle window, int *res)
{
	printf("osGetWindowRect\n");
}

void osSetWindowResizeable(WindowHandle window, int resizeable)
{
	printf("osSetWindowResizeable\n");
}

void osForceContainerReLayout(WindowHandle window)
{
	for (;;) {
		window = window.superview;
		if (window == NULL)
			break;

		handleContainerReLayout(window);
	}
}

void osReLayoutContainer(WindowHandle window)
{
	printf("osReLayoutContainer\n");
}

void osSetDialogMinSize(WindowHandle dialog, int w, int h)
{
	printf("osSetDialogMinSize\n");
}
