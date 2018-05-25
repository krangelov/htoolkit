#include "Button.h"
#include "Internals.h"
#include "Handlers_stub.h"


WindowHandle osCreateButton(WindowHandle window)
{
	GtkWidget *button;

	button = gtk_button_new_with_mnemonic("");
	gtk_signal_connect (GTK_OBJECT(button), "clicked",
			GTK_SIGNAL_FUNC(handleControlCommand),
			NULL);
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), button);

	return button;
};

void osGetButtonReqSize(WindowHandle button, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(button, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}

char *osGetButtonText(WindowHandle button)
{
 	return fromMnemonicString(gtk_button_get_label(GTK_BUTTON(button)));
};

void osSetButtonText(WindowHandle button, char *txt)
{
	gchar *szText = toMnemonicString(txt);
	gtk_button_set_label(GTK_BUTTON(button), szText);
	rfree(szText);
	osForceContainerReLayout(button);
};

void osChangeButtonFont(WindowHandle button, FontHandle font)
{
	gtk_widget_modify_font(button, font->font_descr);
	osForceContainerReLayout(button);
};
