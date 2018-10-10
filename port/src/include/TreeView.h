#ifndef TREEVIEW_H
#define TREEVIEW_H

#include "Types.h"

WindowHandle osCreateTreeView(WindowHandle window);
int osAddTreeViewColumn(WindowHandle treeview, PortString title, int type);
RowHandle osAppendTreeViewItem(WindowHandle treeview);
void osGetTreeViewReqSize(WindowHandle treeview, int *res);

#endif
