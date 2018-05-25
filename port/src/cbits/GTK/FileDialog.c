#include "CommonDialogs.h"
#include "Internals.h"

char *osSelectDirectory(char *title, WindowHandle owner)
{
	GtkWidget *dialog;
	char *filename;

	if (owner)
		owner = gtk_widget_get_toplevel(owner);
	else
		owner = gFrameWidget;

	dialog = gtk_file_chooser_dialog_new (title,
                                      GTK_WINDOW(owner),
                                      GTK_FILE_CHOOSER_ACTION_SAVE,
                                      GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                      NULL);
	gtk_file_chooser_set_action(GTK_FILE_CHOOSER(dialog), GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
	else
		filename = NULL;

	gtk_widget_destroy (dialog);

	return filename;
}

static void SetFileChooserFilter(GtkFileChooser *chooser, char *filter)
{
	for (;;)
	{
		GtkFileFilter *ffilter;
		char *name, *exts, *ext, *s;

		name = filter;
		exts = name + strlen(name)+1;
		filter = exts + strlen(exts)+1;

		if (*name == 0 || *exts == 0)
			break;

		ffilter = gtk_file_filter_new();
		gtk_file_filter_set_name(ffilter, name);

		s = exts;
		for (;;)
		{
			ext = s;
			while (*s != ';' && *s != 0)
				s++;

			if (*s)
			{
				*(s++) = 0;
				gtk_file_filter_add_pattern(ffilter, ext);
			}
			else
			{
				gtk_file_filter_add_pattern(ffilter, ext);
				break;
			}
		}

		gtk_file_chooser_add_filter(chooser, ffilter);
	}
}

char *osSelectInputFile(char *title, char *filter, WindowHandle owner)
{
	GtkWidget *dialog;
	char *filename;

	if (owner)
		owner = gtk_widget_get_toplevel(owner);
	else
		owner = gFrameWidget;

	dialog = gtk_file_chooser_dialog_new (title,
                                      GTK_WINDOW(owner),
                                      GTK_FILE_CHOOSER_ACTION_SAVE,
                                      GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                      NULL);
	gtk_file_chooser_set_action(GTK_FILE_CHOOSER(dialog), GTK_FILE_CHOOSER_ACTION_OPEN);
	SetFileChooserFilter(GTK_FILE_CHOOSER(dialog), filter);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
	else
		filename = NULL;

	gtk_widget_destroy (dialog);

	return filename;
}

char *osSelectInputFiles(char *title, char *filter, WindowHandle owner)
{
	GtkWidget *dialog;
	char *buffer;

	if (owner)
		owner = gtk_widget_get_toplevel(owner);
	else
		owner = gFrameWidget;

	dialog = gtk_file_chooser_dialog_new (title,
                                      GTK_WINDOW(owner),
                                      GTK_FILE_CHOOSER_ACTION_SAVE,
                                      GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                      NULL);
	gtk_file_chooser_set_action(GTK_FILE_CHOOSER(dialog), GTK_FILE_CHOOSER_ACTION_OPEN);
	gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(dialog), TRUE);

	SetFileChooserFilter(GTK_FILE_CHOOSER (dialog), filter);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
	{
		int buffer_size;
		GSList *lst, *files_list;

		files_list = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER(dialog));

		buffer_size = 1;
		lst = files_list;
		while (lst)
		{
			buffer_size += strlen((char *) lst->data)+1;
			lst = lst->next;
		}

		buffer   = malloc(buffer_size);
		if (!buffer)
			return NULL;

		buffer_size = 0;
		lst = files_list;
		while (lst)
		{
			strcpy(buffer+buffer_size,(char *) lst->data);
			buffer_size += strlen((char *) lst->data)+1;
			lst = lst->next;
		}
		*(buffer+buffer_size) = 0;

		g_slist_free(files_list);
	}
	else
		buffer = NULL;

	gtk_widget_destroy (dialog);

	return buffer;
}

char *osSelectOutputFile(char *title, char *filter, char *nameptr, WindowHandle owner)
{
	GtkWidget *dialog;
	char *filename;

	if (owner)
		owner = gtk_widget_get_toplevel(owner);
	else
		owner = gFrameWidget;

	dialog = gtk_file_chooser_dialog_new (title,
                                      GTK_WINDOW(owner),
                                      GTK_FILE_CHOOSER_ACTION_SAVE,
                                      GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                      NULL);
	gtk_file_chooser_set_action(GTK_FILE_CHOOSER(dialog), GTK_FILE_CHOOSER_ACTION_SAVE);
	SetFileChooserFilter(GTK_FILE_CHOOSER (dialog), filter);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
	else
		filename = NULL;

	gtk_widget_destroy (dialog);

	return filename;
}
