#include "PopUp.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreatePopUp(WindowHandle window)
{
	GtkWidget *popup;

	popup =  gtk_combo_box_new_text();
	//gtk_entry_set_editable (GTK_ENTRY(GTK_COMBO(popup)->entry), gtk_false());
	//gtk_combo_set_use_arrows_always(GTK_COMBO(popup), gtk_true());
	gtk_signal_connect(GTK_OBJECT(popup),
		"changed",
		GTK_SIGNAL_FUNC(handleControlCommand),
		NULL);
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), popup);

	return popup;
};

void osAppendPopUpItem(WindowHandle popup, char *title)
{
	gtk_combo_box_append_text(GTK_COMBO_BOX(popup), title);
};

void osInsertPopUpItem(WindowHandle popup, int index, char *title)
{
	gtk_combo_box_insert_text(GTK_COMBO_BOX(popup), index, title);
};

void osRemovePopUpItem(WindowHandle popup, int index)
{
	gtk_combo_box_remove_text(GTK_COMBO_BOX(popup), index);
};

void osRemoveAllPopUpItems(WindowHandle popup)
{
	gtk_list_store_clear(GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(popup))));
};

void osGetPopUpReqSize(WindowHandle popup, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(popup, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
};

int osGetPopUpSelection(WindowHandle popup)
{
	   return gtk_combo_box_get_active(GTK_COMBO_BOX(popup));
};

void osSetPopUpSelection(WindowHandle popup, int index)
{
	gtk_combo_box_set_active(GTK_COMBO_BOX(popup), index);
};
