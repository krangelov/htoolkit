#ifndef COMMONDIALOGS_H
#define COMMONDIALOGS_H

#include "Types.h"

char *osSelectDirectory(char *title, WindowHandle owner);
char *osSelectInputFile(char *title, char *filter, WindowHandle owner);
char *osSelectInputFiles(char *title, char *filter, WindowHandle owner);
char *osSelectOutputFile(char *title, char *filter, char *nameptr, WindowHandle owner);

BOOL osRunColorDialog(unsigned int *color, WindowHandle owner);

BOOL osRunFontDialog(char **fname, int *fsize, int *fweight, int *fstyle, BOOL *funderline, BOOL *fstrikeout, WindowHandle owner);

void osRunAboutDialog(char *appName, char *appVersion, char *copyright, char *comments, char *authors, char *documenters, char *translator_credits, BitmapHandle bitmap, WindowHandle owner);

#endif
