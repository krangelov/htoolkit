#include "Button.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateButton(WindowHandle window)
{
	NSButton *button = [[NSButton alloc] initWithFrame: NSMakeRect(30, 30, 80, 30)]; 
	[button setAction: @selector(controlCommand:)];
	[button setTarget: [NSApp delegate]];

	[window addSubview: button];

	return button;
};

void osGetButtonReqSize(WindowHandle button, int *res)
{
	NSSize size = [button intrinsicContentSize];
	res[0] = size.width;
	res[1] = size.height;
}

char *osGetButtonText(WindowHandle button)
{
  	return strdup([[((NSButton*) button) title] UTF8String]);
};

void osSetButtonText(WindowHandle button, char *txt)
{
	[((NSButton*) button) setTitle: [NSString stringWithUTF8String: txt]];
	osForceContainerReLayout(button);
};

void osChangeButtonFont(WindowHandle button, FontHandle font)
{
	[((NSControl*) button) setFont: font];
	osForceContainerReLayout(button);
};
