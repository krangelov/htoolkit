#include "Types.h"
#include "ConfigKey.h"
#include "Internals.h"
#include "Handlers_stub.h"

int gDocumentInterface = 0;
NSWindow *gMainWindow;
char *gAppTitle = NULL;
char *gAppName = NULL;
char *gAppVersion = NULL;

extern void hs_free_fun_ptr    (HsFunPtr fp);

@interface AppController : NSObject
{
	OsInitFunc *initFunc;
}


- (void)menuItemClicked: (NSMenuItem *) item;
- (void)applicationWillFinishLaunching:(NSNotification *) not;

@end

@implementation AppController
- (id)init: (OsInitFunc) func {
    if (self = [super init]) {
		initFunc = func;
        return self;
    } else
        return nil;
}

- (void) applicationWillFinishLaunching: (NSNotification *) not
{
	NSMenu* mainMenu = [NSMenu new];

	NSMenuItem *item =
		[mainMenu addItemWithTitle: [NSString stringWithUTF8String: gAppName]
		                    action: NULL
		             keyEquivalent: @""];

	NSMenu *appMenu = [NSMenu new];
	[appMenu addItemWithTitle: [NSString stringWithFormat: @"Hide %s", gAppName]
					   action: @selector(hide:)
				keyEquivalent: @"h"];
	[[appMenu addItemWithTitle: @"Hide Others"
					   action: @selector(hideOtherApplications:)
				keyEquivalent: @"h"] setKeyEquivalentModifierMask: NSAlternateKeyMask | NSCommandKeyMask];
    [appMenu addItem: [NSMenuItem separatorItem]];
	[appMenu addItemWithTitle: [NSString stringWithFormat: @"Quit %s", gAppName]
					   action: @selector(terminate:)
				keyEquivalent: @"q"];

	[mainMenu setSubmenu: appMenu
	             forItem: item];

	[NSApp setMainMenu: mainMenu];

	[mainMenu release];

	if (initFunc != NULL) {
		initFunc();
		hs_free_fun_ptr(initFunc);
		initFunc = NULL;
	}
}

- (void) applicationDidFinishLaunching: (NSNotification *) not
{
	NSMenuItem *item = [[NSApp mainMenu] itemAtIndex: 0];

	NSString *nsTitle = [NSString stringWithUTF8String: gAppName];
	[item setTitle: nsTitle];
	[item.submenu setTitle: nsTitle];
}

- (void)hideOtherApplications: (NSMenuItem*) item
{
	[[NSWorkspace sharedWorkspace] hideOtherApplications];
}

- (void)menuItemClicked: (NSMenuItem*) item
{
	handleActionCommand(item.representedObject);
}

- (void)timerClicked: (NSTimer *) timer
{
	handleTimer(timer);
}

- (void)controlCommand: (NSControl *) control
{
	handleControlCommand(control);
}
@end

void parseAppNameVersion(char *appTitle,char *appVersion)
{
	int appNameLen;
	char *s;

	s = appVersion;
	while (*s != ' ' && *s != 0) s++;

	if (*s == ' ')
	{
		appNameLen = s-appVersion;
		gAppName = malloc(appNameLen+1);
		memcpy(gAppName, appVersion, appNameLen);
		gAppName[appNameLen] = 0;

		while (*s == ' ') s++;
		gAppVersion = strdup(s);
	}
	else
	{
		gAppName    = strdup(appTitle);
		gAppVersion = strdup(appVersion);
	}

	s = gAppName;
	appNameLen = 0;
	while (*s)
	{
		if (isalnum(*s))
			gAppName[appNameLen++] = *s++;
		else
			s++;
	}
	gAppName[appNameLen] = 0;

	gAppTitle   = strdup(appTitle);
}

void osStart(char *appTitle, char *appVersion, int DocumentInterface, OsInitFunc *initFunc)
{
	parseAppNameVersion(appTitle,appVersion);

	NSAutoreleasePool *pool;
	AppController *delegate;
   
	pool = [[NSAutoreleasePool alloc] init];
	delegate = [[AppController alloc] init: initFunc];

	[NSApplication sharedApplication];
	[NSApp setDelegate: delegate];

	[pool release];

	gDocumentInterface = DocumentInterface;
	gMainWindow = NULL;

	[NSApp run];

	osDestroyAllActions();
	handleProcessDestroy();
	free(gAppTitle);   gAppTitle = NULL;
	free(gAppName);    gAppName = NULL;
	free(gAppVersion); gAppVersion = NULL;
}

void osQuit()
{
	[NSApp terminate: nil];
}
