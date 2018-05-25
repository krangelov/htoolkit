#include "CheckBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateCheckBox(WindowHandle window)
{
	GtkWidget *check_btn;

	check_btn = gtk_check_button_new_with_mnemonic("");
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), check_btn);
	gtk_signal_connect (GTK_OBJECT (check_btn), "toggled",
			GTK_SIGNAL_FUNC(handleControlCommand),
			NULL);

	return check_btn;
};

void osGetCheckBoxReqSize(WindowHandle checkbox, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(checkbox, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
};

char *osGetCheckBoxText(WindowHandle button)
{
	return fromMnemonicString(gtk_button_get_label(GTK_BUTTON(button)));
};

void osSetCheckBoxText(WindowHandle button, char *txt)
{
	gchar *szText = toMnemonicString(txt);
	gtk_button_set_label(GTK_BUTTON(button), szText);
	rfree(szText);
	osForceContainerReLayout(button);
};

BOOL osGetCheckBoxState(WindowHandle checkbox)
{
	return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbox));
};

void osSetCheckBoxState(WindowHandle checkbox, BOOL state)
{
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), state);
};
