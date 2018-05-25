#include "DateEntry.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateDateEntry(WindowHandle window)
{
	GtkWidget *entry;

	entry =  gnome_date_edit_new(0,FALSE,FALSE);
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), entry);

	return entry;
};

void osGetDateEntryReqSize(WindowHandle entry, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(entry, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}

time_t osGetDateEntryValue(WindowHandle entry)
{
	return gnome_date_edit_get_time(GNOME_DATE_EDIT(entry));
}

void osSetDateEntryValue(WindowHandle entry, time_t value)
{
	 gnome_date_edit_set_time(GNOME_DATE_EDIT(entry), value);
}
