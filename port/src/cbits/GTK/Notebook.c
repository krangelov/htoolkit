#include "Notebook.h"
#include "Internals.h"
#include "Handlers_stub.h"

static void notebook_switch_page_handler(GtkNotebook *notebook,GtkNotebookPage *page,gint page_num,gpointer user_data)
{
	// send deactivate message for old
	gint old_page_num = g_list_index(notebook->children, notebook->cur_page);
	handleWindowDeactivate(gtk_notebook_get_nth_page(notebook, old_page_num));

	// send activate message for new
	handleWindowActivate(gtk_notebook_get_nth_page(notebook, page_num));
}

WindowHandle osCreateNotebook(WindowHandle window)
{
	GtkWidget *notebook;

	notebook = gtk_notebook_new();
	gtk_signal_connect (GTK_OBJECT(window), "switch-page",
			GTK_SIGNAL_FUNC(notebook_switch_page_handler),
			NULL);
	gtk_notebook_set_scrollable(GTK_NOTEBOOK(notebook), gtk_true());
	gtk_notebook_popup_enable(GTK_NOTEBOOK(notebook));
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), notebook);

	return notebook;
};

void osGetNotebookReqSize(WindowHandle widget, int *res)
{
	GtkWidget *page, *label;
	GList *children;
	GtkRequisition child_requisition;
	gint focus_width;
	gint tab_width, tab_height;
	GtkNotebook *notebook = GTK_NOTEBOOK (widget);

	gtk_widget_style_get (widget, "focus-line-width", &focus_width, NULL);

	tab_width  = 0;
	tab_height = 0;

	children = gtk_container_get_children(GTK_CONTAINER(notebook));
	while (children)
	{
		page = children->data;

		if (GTK_WIDGET_VISIBLE (page))
		{
			label = gtk_notebook_get_tab_label(notebook, page);
			gtk_widget_size_request (label, &child_requisition);

			child_requisition.width  += 2 * widget->style->xthickness;
			child_requisition.height += 2 * widget->style->ythickness;

			switch (notebook->tab_pos)
			{
			case GTK_POS_TOP:
			case GTK_POS_BOTTOM:
				child_requisition.height += 2 * (notebook->tab_vborder + focus_width);
				tab_height = MAX (tab_height, child_requisition.height);
				break;
			case GTK_POS_LEFT:
			case GTK_POS_RIGHT:
				child_requisition.width += 2 * (notebook->tab_hborder + focus_width);
				tab_width = MAX (tab_width, child_requisition.width);
				break;
			}
		}

		children = g_list_remove(children, page);
	}

	res[0] = (widget->style->xthickness + GTK_CONTAINER (widget)->border_width) * 2 + tab_width;
	res[1] = (widget->style->ythickness + GTK_CONTAINER (widget)->border_width) * 2 + tab_height;
}

void osSetNotebookLabelsPosition(WindowHandle notebook, PositionType position)
{
	switch (position)
	{
	case PosLeft:   gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_LEFT);   break;
	case PosTop:    gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);    break;
	case PosRight:  gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_RIGHT);  break;
	case PosBottom: gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_BOTTOM); break;
	}
}

PositionType osGetNotebookLabelsPosition(WindowHandle notebook)
{
	switch (gtk_notebook_get_tab_pos(GTK_NOTEBOOK(notebook)))
	{
	case GTK_POS_LEFT:   return PosLeft;
	case GTK_POS_TOP:    return PosTop;
	case GTK_POS_RIGHT:  return PosRight;
	case GTK_POS_BOTTOM: return PosBottom;
	}

	return 0;
}

int osGetNotebookSelection(WindowHandle notebook)
{
	return gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
};

void osSetNotebookSelection(WindowHandle notebook, int index)
{
	gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), index);
};

WindowHandle osInsertNotebookPage(WindowHandle notebook, int pos)
{
	GtkWidget *hbox;
	GtkWidget *page = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(page),
					  GTK_POLICY_NEVER, GTK_POLICY_NEVER);
	gtk_container_add(GTK_CONTAINER(page), port_layout_new(NULL,NULL));
	gtk_widget_show_all(page);

	hbox = gtk_hbox_new(FALSE, 1);
	gtk_box_pack_end(GTK_BOX(hbox), gtk_label_new(""), TRUE, TRUE, 0);
	gtk_widget_show_all(hbox);

	if (pos < 0)
		gtk_notebook_append_page(GTK_NOTEBOOK(notebook),page,hbox);
	else
		gtk_notebook_insert_page(GTK_NOTEBOOK(notebook),page,hbox,pos);
	return page;
}

char *osGetNotebookPageTitle(WindowHandle page)
{
	GList *children;
	GtkWidget *notebook, *tab_label;

	notebook = gtk_widget_get_parent(page);
	tab_label = gtk_notebook_get_tab_label(GTK_NOTEBOOK(notebook),page);
	children = gtk_container_get_children(GTK_CONTAINER(tab_label));

	return strdup(gtk_label_get_text(GTK_LABEL(g_list_last(children)->data)));
};

void osSetNotebookPageTitle(WindowHandle page, char *txt)
{
	GList *children;
	GtkWidget *notebook, *tab_label;

	notebook = gtk_widget_get_parent(page);
	tab_label = gtk_notebook_get_tab_label(GTK_NOTEBOOK(notebook),page);
	children = gtk_container_get_children(GTK_CONTAINER(tab_label));

	return gtk_label_set_text(GTK_LABEL(g_list_last(children)->data), txt);
};

int osGetNotebookPagePos(WindowHandle page)
{
	return gtk_notebook_page_num(GTK_NOTEBOOK(gtk_widget_get_parent(page)), page);
}

void osDestroyNotebookPage(WindowHandle page)
{
	gtk_widget_destroy(page);
}

void osGetNotebookPageSize(WindowHandle page, int *res)
{
	res[0] = page->allocation.width-4;
	res[1] = page->allocation.height-4;
}

void osSetNotebookPageBitmap(WindowHandle page, BitmapHandle bitmap)
{
	GList *children;
	GtkNotebook *notebook;
	GtkWidget* tab_label, *image;

	notebook = GTK_NOTEBOOK(gtk_widget_get_parent(page));
	tab_label = gtk_notebook_get_tab_label(notebook,page);
	children = gtk_container_get_children(GTK_CONTAINER(tab_label));

	if (bitmap)
	{
		if (g_list_length(children) == 1)
		{
			image = gtk_image_new();
			gtk_box_pack_start(GTK_BOX(tab_label), image, TRUE, TRUE, 0);
			gtk_widget_show(image);
		}
		else
		{
			image = GTK_WIDGET(g_list_first(children)->data);
		}

		gtk_image_set_from_pixbuf(GTK_IMAGE(image), bitmap->pixbuf);
	}
	else
	{
		if (g_list_length(children) == 2)
		{
			image = GTK_WIDGET(g_list_first(children)->data);
			gtk_container_remove(GTK_CONTAINER(tab_label), image);
		}
	}
}

int osGetNotebookPageCount(WindowHandle notebook)
{
	return gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook));
}
