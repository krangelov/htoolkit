#include "RadioBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateRadioBox(WindowHandle window)
{
	GtkWidget *radio_btn;

	radio_btn = gtk_radio_button_new_with_mnemonic (NULL, "");
	GTK_TOGGLE_BUTTON (radio_btn)->active = FALSE;

	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), radio_btn);
	gtk_signal_connect (GTK_OBJECT (radio_btn), "toggled",
			GTK_SIGNAL_FUNC(handleControlCommand),
			NULL);

	return radio_btn;
};

void osGetRadioBoxReqSize(WindowHandle radio, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(radio, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
};

char *osGetRadioBoxText(WindowHandle button)
{
	return fromMnemonicString(gtk_button_get_label(GTK_BUTTON(button)));
};

void osSetRadioBoxText(WindowHandle button, char *txt)
{
	gchar *szText = toMnemonicString(txt);
	gtk_button_set_label(GTK_BUTTON(button), szText);
	rfree(szText);
	osForceContainerReLayout(button);
};

BOOL osGetRadioBoxState(WindowHandle radio)
{
	return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(radio));
};

void osSetRadioBoxState(WindowHandle radio, BOOL state)
{
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), state);
	GTK_TOGGLE_BUTTON(radio)->active = state;
};

void osSetRadioBoxGroup(WindowHandle *handles)
{
	WindowHandle *phandle;
	GSList *group = NULL;

	for (phandle = handles; *phandle; phandle++)
	{
		gtk_radio_button_set_group(GTK_RADIO_BUTTON(*phandle), group);
		group = gtk_radio_button_get_group(GTK_RADIO_BUTTON(*phandle));
	}
}
