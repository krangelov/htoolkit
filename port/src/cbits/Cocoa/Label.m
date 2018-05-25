#include "Label.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateLabel(WindowHandle window)
{
	NSTextField *label = [[NSTextField alloc] initWithFrame: NSMakeRect(30, 30, 80, 30)]; 
	[label setSelectable: NO];
	[label setBezeled: NO];
	[label setDrawsBackground: NO];
	//[label setBackgroundColor: [NSColor blueColor]];

	[window addSubview: label];

	return label;
};

void osGetLabelReqSize(WindowHandle label, int *res)
{
	NSSize size = [label intrinsicContentSize];
	res[0] = size.width;
	res[1] = size.height;
}

char *osGetLabelText(WindowHandle label)
{
  	return strdup([[((NSControl*) label) stringValue] UTF8String]);
};

void osSetLabelText(WindowHandle label, char *txt)
{
	[((NSControl*) label) setStringValue: [NSString stringWithUTF8String: txt]];
	osForceContainerReLayout(label);
};

void osChangeLabelFont(WindowHandle label, FontHandle font)
{
	[((NSControl*) label) setFont: font];
	osForceContainerReLayout(label);
};
