#include "EditBox.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateEdit(WindowHandle window)
{
	GtkWidget *entry;

	entry = gtk_entry_new();
	GTK_ENTRY(entry)->is_cell_renderer = TRUE;
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), entry);

	return entry;
};

void osGetEditReqSize(WindowHandle edit, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(edit, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}

char *osGetEditText(WindowHandle entry)
{
	return strdup(gtk_entry_get_text(GTK_ENTRY(entry)));
};

void osSetEditText(WindowHandle entry, char *txt)
{
	gtk_entry_set_text(GTK_ENTRY(entry), txt);
};

void osSetEditReadOnly(WindowHandle entry, BOOL readOnly)
{
	gtk_editable_set_editable(GTK_EDITABLE(entry), !readOnly);
}

BOOL osGetEditReadOnly(WindowHandle entry)
{
	return !gtk_editable_get_editable(GTK_EDITABLE(entry));
}

void osSetEditPassword(WindowHandle entry, BOOL password)
{
	gtk_entry_set_visibility(GTK_ENTRY(entry), !password);
}

BOOL osGetEditPassword(WindowHandle entry)
{
	return !gtk_entry_get_visibility(GTK_ENTRY(entry));
}

void osChangeEditBoxFont(WindowHandle entry, FontHandle font)
{
	gtk_widget_modify_font(entry, font->font_descr);
	osForceContainerReLayout(entry);
};
