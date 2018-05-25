#include "CommonDialogs.h"
#include "Internals.h"

const gchar **strList2Array(char *list)
{
	int count, i;
	gchar *s, **array;
	
	if (!list) return NULL;

	count = 0;
	for (s = list; *s; s+=strlen(s)+1) count++;

	array = (char**) malloc((count+1)*sizeof(gchar*));
	if (!array)
		return NULL;

	i = 0;
	for (s = list; *s; s+=strlen(s)+1) array[i++] = s;
	array[i] = NULL;

	return (const gchar**) array;
}

void osRunAboutDialog(char *appName, char *appVersion, char *copyright, char *comments, char *authors, char *documenters, char *translator_credits, BitmapHandle bitmap, WindowHandle owner)
{
	GtkWidget *about;
	const gchar **authorsArr, **documentersArr;

	authorsArr     = strList2Array(authors);
	documentersArr = strList2Array(documenters);

	const gchar* null_string = NULL;
	if (authorsArr == NULL)
		authorsArr = &null_string;

	about = gnome_about_new(appName,
	                        appVersion,
	                        copyright,
	                        comments,
	                        authorsArr,
	                        documentersArr,
	                        translator_credits,
	                        bitmap->pixbuf);

	if (owner)
		owner = gtk_widget_get_toplevel(owner);
	else
		owner = gFrameWidget;
	gtk_window_set_transient_for(GTK_WINDOW(about), GTK_WINDOW(owner));

	gtk_dialog_run(GTK_DIALOG(about));

	free(documentersArr);
	
	if (authorsArr != &null_string)
		free(authorsArr);
}
