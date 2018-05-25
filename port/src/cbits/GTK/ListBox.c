#include "ListBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

static void tree_view_selection_chanded(GtkWidget *w, gpointer user_data)
{
	handleControlCommand((WindowHandle) user_data);
}

WindowHandle osCreateListBox(WindowHandle window, BOOL multisel)
{
	GtkListStore *store;
	GtkWidget *lbox, *sw;
	GtkTreeViewColumn *column;

	/* create scrolled window */
	sw = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (sw), GTK_SHADOW_ETCHED_IN);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
			GTK_POLICY_AUTOMATIC,
			GTK_POLICY_AUTOMATIC);

	/* create list store */
	store = gtk_list_store_new (1, G_TYPE_STRING);

	/* create tree view */
	lbox = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(lbox), FALSE);
	gtk_tree_view_set_enable_search (GTK_TREE_VIEW(lbox), TRUE);
	gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(lbox)),
		multisel ? GTK_SELECTION_MULTIPLE : GTK_SELECTION_SINGLE);
	g_object_unref (store);

	/* add column to the tree view */
	column = gtk_tree_view_column_new_with_attributes ("",
				 gtk_cell_renderer_text_new (),
				"text",
				0,
				NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW(lbox), column);

	gtk_container_add(GTK_CONTAINER(sw), lbox);
	gtk_widget_show(lbox);

	g_signal_connect (G_OBJECT(gtk_tree_view_get_selection(GTK_TREE_VIEW(lbox))), "changed",
		GTK_SIGNAL_FUNC(tree_view_selection_chanded),
		sw);

	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), sw);

	return sw;
};

static void checkbox_toggled(GtkCellRendererToggle *cellrenderertoggle, gchar *path_string, gpointer user_data)
{
	GtkWidget *sw = (GtkWidget *)user_data;
	GtkWidget *listbox = GTK_BIN(sw)->child;
	GtkTreeModel *model;
	GtkTreeIter iter;
	GtkTreePath *path;
	gboolean state;

	path = gtk_tree_path_new_from_string (path_string);
	model = gtk_tree_view_get_model(GTK_TREE_VIEW(listbox));
	gtk_tree_model_get_iter (model, &iter, path);

	gtk_tree_model_get(model, &iter, 0, &state, -1);
	gtk_list_store_set (GTK_LIST_STORE (model), &iter, 0, !state, -1);

	handleControlCommand(sw);
};

WindowHandle osCreateCheckListBox(WindowHandle window)
{
	GtkListStore *store;
	GtkWidget *lbox, *sw;
	GtkTreeViewColumn *column;
	GtkCellRenderer *render;

	/* create scrolled window */
	sw = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (sw), GTK_SHADOW_ETCHED_IN);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
			GTK_POLICY_AUTOMATIC,
			GTK_POLICY_AUTOMATIC);

	/* create list store */
	store = gtk_list_store_new (2, G_TYPE_BOOLEAN, G_TYPE_STRING);

	/* create tree view */
	lbox = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(lbox), FALSE);
	gtk_tree_view_set_enable_search (GTK_TREE_VIEW(lbox), TRUE);
	gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(lbox)), GTK_SELECTION_SINGLE);
	g_object_unref (store);

	/* add columns to the tree view */
	render = gtk_cell_renderer_toggle_new ();
	g_signal_connect (render, "toggled",
		    G_CALLBACK (checkbox_toggled), sw);
	column = gtk_tree_view_column_new_with_attributes ("",
				render,
				"active",
				0,
				NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW(lbox), column);
	column = gtk_tree_view_column_new_with_attributes ("",
				gtk_cell_renderer_text_new (),
				"text",
				1,
				NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW(lbox), column);

	gtk_container_add(GTK_CONTAINER(sw), lbox);
	gtk_widget_show(lbox);

	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), sw);

	return sw;
};

void osAppendListBoxItem(WindowHandle listbox, char *title)
{
	GtkTreeIter iter;
	GtkWidget *lbox = GTK_BIN(listbox)->child;
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(lbox));
	gtk_list_store_append(GTK_LIST_STORE(model), &iter);

	if (gtk_tree_model_get_n_columns(model) > 1)
		gtk_list_store_set(GTK_LIST_STORE(model), &iter, 0, FALSE, 1, title, -1);
	else
		gtk_list_store_set(GTK_LIST_STORE(model), &iter, 0, title, -1);
};

void osInsertListBoxItem(WindowHandle listbox, int index, char *title)
{
	GtkTreeIter iter;
	GtkWidget *lbox = GTK_BIN(listbox)->child;
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(lbox));
	gtk_list_store_insert(GTK_LIST_STORE(model), &iter, index);

	if (gtk_tree_model_get_n_columns(model) > 1)
		gtk_list_store_set(GTK_LIST_STORE(model), &iter, 0, FALSE, 1, title, -1);
	else
		gtk_list_store_set(GTK_LIST_STORE(model), &iter, 0, title, -1);
};

void osRemoveListBoxItem(WindowHandle listbox, int index)
{
	GtkTreeIter iter;
	GtkWidget *lbox;
	GtkTreeModel *model;
	GtkTreePath *path;

	lbox = GTK_BIN(listbox)->child;
	model = gtk_tree_view_get_model(GTK_TREE_VIEW(lbox));
	path = gtk_tree_path_new_from_indices(index, -1);
	if (gtk_tree_model_get_iter(model,&iter,path))
		gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
	gtk_tree_path_free(path);
};

void osRemoveAllListBoxItems(WindowHandle listbox)
{
	gtk_list_store_clear(GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(GTK_BIN(listbox)->child))));
};

void osGetListBoxReqSize(WindowHandle listbox, int *res)
{
	res[0] = 80;
	res[1] = 60;
};

int osGetListBoxSingleSelection(WindowHandle listbox)
{
	GtkTreeIter iter;
	GtkTreePath *path;
	GtkTreeModel *model = NULL;
	GtkWidget *lbox = GTK_BIN(listbox)->child;
	int index = -1;

	if (gtk_tree_selection_get_selected (gtk_tree_view_get_selection(GTK_TREE_VIEW(lbox)), &model, &iter))
	{
		path = gtk_tree_model_get_path(model, &iter);
		if (gtk_tree_path_get_depth(path) >= 1)
			index = gtk_tree_path_get_indices(path)[0];
		gtk_tree_path_free(path);
	}

	return index;
};

BOOL osGetListBoxItemSelectState(WindowHandle listbox, int index)
{
	BOOL result;
	GtkWidget *lbox = GTK_BIN(listbox)->child;
	GtkTreePath *path = gtk_tree_path_new_from_indices (index, -1);
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(lbox));

	if (gtk_tree_model_get_n_columns(model) > 1)
	{
		GtkTreeIter iter;
		gtk_tree_model_get_iter (model, &iter, path);
		gtk_tree_model_get(model, &iter, 0, &result, -1);
	}
	else
	{
		GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(lbox));
		result =  gtk_tree_selection_path_is_selected(selection, path);
	}

	gtk_tree_path_free(path);
	return result;
}

void osSetListBoxSingleSelection(WindowHandle listbox, int index)
{
	GtkWidget *lbox = GTK_BIN(listbox)->child;
	GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(lbox));

	if (index >= 0)
	{
		GtkTreePath *path = gtk_tree_path_new_from_indices (index, -1);
		gtk_tree_view_set_cursor(GTK_TREE_VIEW(lbox), path, NULL, TRUE);
		gtk_tree_selection_select_path(selection, path);
		gtk_tree_path_free(path);
	}
	else
		gtk_tree_selection_unselect_all (selection);
};

void osSetListBoxItemSelectState(WindowHandle listbox, int index, BOOL state)
{
	GtkWidget *lbox = GTK_BIN(listbox)->child;
	GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(lbox));

	if (index >= 0)
	{
		GtkTreePath *path = gtk_tree_path_new_from_indices (index, -1);
		GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(lbox));

		if (gtk_tree_model_get_n_columns(model) > 1)
		{
			GtkTreeIter iter;
			gtk_tree_model_get_iter (model, &iter, path);
			gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0, state, -1);
		}
		else
		{
			if (state)
				gtk_tree_selection_select_path(selection, path);
			else
				gtk_tree_selection_unselect_path(selection, path);
		}

		gtk_tree_path_free(path);
	}
};

int osGetListBoxCurrentItem(WindowHandle listbox)
{
	GtkTreePath *path;
	GtkWidget *lbox = GTK_BIN(listbox)->child;

	gtk_tree_view_get_cursor(GTK_TREE_VIEW(lbox), &path, NULL);
	if (path != NULL && gtk_tree_path_get_depth(path) >= 1)
		return gtk_tree_path_get_indices(path)[0];
	else
		return -1;
}
