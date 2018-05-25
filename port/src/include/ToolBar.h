#ifndef TOOLBAR_H
#define TOOLBAR_H

#include "Types.h"

WindowHandle osCreateToolBar(char *name, PositionType place, int band_num, int band_position, int offset);
void osDestroyToolBar(WindowHandle toolbar);
int osGetToolBarButtonCount(WindowHandle toolbar);

ToolHandle osInsertToolButton(ActionHandle action, WindowHandle toolbar, int pos);
ToolHandle osInsertToolLine(WindowHandle toolbar, int pos);
void osDestroyToolItem(ToolHandle toolItem);
int osGetToolItemPos(ToolHandle toolItem);

#endif
