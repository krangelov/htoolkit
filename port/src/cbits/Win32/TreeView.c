#include "TreeView.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateTreeView(WindowHandle window)
{
	printf("osCreateTreeView is not implemented\n");
	return NULL;
}

int osAddTreeViewColumn(WindowHandle treeview, PortString title, int type)
{
	printf("osAddTreeViewColumn is not implemented\n");
	return 0;
}

RowHandle osAppendTreeViewItem (WindowHandle treeview, RowHandle parent)
{
	printf("osAppendTreeViewItem is not implemented\n");
	return NULL;
}

void osGetTreeViewReqSize(WindowHandle treeview, int *res)
{
	printf("osGetTreeViewReqSize is not implemented\n");
}
