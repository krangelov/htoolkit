#include "Message.h"
#include "Internals.h"

void osMessageAlert(char *szText)
{
	GtkWidget *msg_dialog;
	msg_dialog = gtk_message_dialog_new(GTK_WINDOW(gFrameWidget), GTK_DIALOG_MODAL, GTK_MESSAGE_INFO, GTK_BUTTONS_OK,
										"%s", szText);
	gtk_dialog_run(GTK_DIALOG(msg_dialog));
	gtk_widget_destroy (msg_dialog);
};

BOOL osMessageConfirm(char *szText)
{
	gboolean result;
	GtkWidget *msg_dialog;
	msg_dialog = gtk_message_dialog_new(GTK_WINDOW(gFrameWidget), GTK_DIALOG_MODAL, GTK_MESSAGE_INFO, GTK_BUTTONS_OK_CANCEL,
										"%s", szText);
	result = (gtk_dialog_run(GTK_DIALOG(msg_dialog)) == GTK_RESPONSE_OK);
	gtk_widget_destroy (msg_dialog);
	return result;
};

void osMessageWarning(char *szText)
{
	GtkWidget *msg_dialog;
	msg_dialog = gtk_message_dialog_new(GTK_WINDOW(gFrameWidget), GTK_DIALOG_MODAL, GTK_MESSAGE_WARNING, GTK_BUTTONS_OK,
										"%s", szText);
	gtk_dialog_run(GTK_DIALOG(msg_dialog));
	gtk_widget_destroy (msg_dialog);
};

BOOL osMessageQuestion(char *szText)
{
	gboolean result;
	GtkWidget *msg_dialog;
	msg_dialog = gtk_message_dialog_new(GTK_WINDOW(gFrameWidget), GTK_DIALOG_MODAL, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO,
										"%s", szText);
	result = (gtk_dialog_run(GTK_DIALOG(msg_dialog)) == GTK_RESPONSE_YES);
	gtk_widget_destroy (msg_dialog);
	return result;
};

BOOL osMessageError(char *szText)
{
	gboolean result;
	GtkWidget *msg_dialog;
	msg_dialog = gtk_message_dialog_new(GTK_WINDOW(gFrameWidget), GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_OK_CANCEL,
										"%s", szText);
	result = (gtk_dialog_run(GTK_DIALOG(msg_dialog)) == GTK_RESPONSE_OK);
	gtk_widget_destroy (msg_dialog);
	return result;
};

int osMessageCancelQuestion(char *szText)
{
	gboolean result;
	GtkWidget *msg_dialog;
	msg_dialog = gtk_message_dialog_new(GTK_WINDOW(gFrameWidget), GTK_DIALOG_MODAL, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO,
										"%s", szText);
	gtk_dialog_add_button (GTK_DIALOG(msg_dialog), GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);

	switch (gtk_dialog_run(GTK_DIALOG(msg_dialog)))
	{
		case GTK_RESPONSE_NO:  	result =  0; break;
		case GTK_RESPONSE_YES: 	result =  1; break;
		default:    			result = -1; break;
	}

	gtk_widget_destroy (msg_dialog);
	return result;
};

int osMessageConfirmSave(char *szText)
{
	gboolean result;
	GtkWidget *msg_dialog;
	msg_dialog = gtk_message_dialog_new(GTK_WINDOW(gFrameWidget), GTK_DIALOG_MODAL, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO,
										"%s", szText);
	gtk_dialog_add_button (GTK_DIALOG(msg_dialog), GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);

	switch (gtk_dialog_run(GTK_DIALOG(msg_dialog)))
	{
		case GTK_RESPONSE_NO:  	result =  0; break;
		case GTK_RESPONSE_YES: 	result =  1; break;
		default:    			result = -1; break;
	}

	gtk_widget_destroy (msg_dialog);
	return result;
}
