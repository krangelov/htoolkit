#include "WebView.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateWebView(WindowHandle window)
{
	printf("osCreateWebView is not implemented\n");
	return NULL;
}

void osWebViewLoadURL(WindowHandle window, PortString url)
{
	printf("osWebViewLoadURL is not implemented\n");
}

void osGetWebViewReqSize(WindowHandle webview, int *res)
{
	printf("osGetWebViewReqSize is not implemented\n");
}
