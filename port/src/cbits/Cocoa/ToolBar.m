#include "ToolBar.h"
#include "Action.h"
#include "Internals.h"
#include "Handlers_stub.h"


WindowHandle osCreateToolBar(char *name, PositionType place, int band_num, int band_position, int offset)
{
	return [[NSToolbar new] initWithIdentifier: [NSString stringWithUTF8String: name]];
}

void osDestroyToolBar(WindowHandle toolbar)
{
	[toolbar release];
}

int osGetToolBarButtonCount(WindowHandle toolbar)
{
	return [[(NSToolbar*)toolbar items] count];
}

ToolHandle osInsertToolButton(ActionHandle action, WindowHandle toolbar, int pos)
{
	printf("osInsertToolButton\n");
	return NULL;
}

ToolHandle osInsertToolLine(WindowHandle toolbar, int pos)
{
	printf("osInsertToolLine\n");
	return NULL;
}

void osDestroyToolItem(ToolHandle toolItem)
{
	printf("osDestroyToolItem\n");
}

int osGetToolItemPos(ToolHandle toolItem)
{
	printf("osDestroyToolItem\n");
	return 0;
}
