#ifndef WEBVIEW_H
#define WEBVIEW_H

#include "Types.h"

WindowHandle osCreateWebView(WindowHandle window);
void osWebViewLoadURL(WindowHandle webview, PortString url);
void osGetWebViewReqSize(WindowHandle webview, int *res);

#endif
