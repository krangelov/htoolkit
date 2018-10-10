#include "WebView.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateWebView(WindowHandle window)
{
	GtkWidget* view;

	view = webkit_web_view_new();
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), view);

	return view;
}

void osWebViewLoadURL(WindowHandle window, PortString url)
{
	webkit_web_view_load_uri(WEBKIT_WEB_VIEW(window), url);
}

void osGetWebViewReqSize(WindowHandle webview, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(webview, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}
