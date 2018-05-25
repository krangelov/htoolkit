#include "Types.h"
#include "ConfigKey.h"
#include "Internals.h"
#include "Handlers_stub.h"

GnomeProgram *gProgram = NULL;
int gDocumentInterface;

void *rmalloc (unsigned long bytes)
{
	void *ptr = malloc (bytes);

	if (!ptr)
	{
		printf("rmalloc failed!\n");
		exit(1);
	}

	return ptr;
}

void rfree (void *ptr)
{
	if (ptr)
		free(ptr);
}

void parseAppNameVersion(char *appTitle,char *appVersion)
{
	int appNameLen;
	char *s;

	s = appVersion;
	while (*s != ' ' && *s != 0) s++;

	if (*s == ' ')
	{
		appNameLen = s-appVersion;
		gAppName = malloc(appNameLen+1);
		memcpy(gAppName, appVersion, appNameLen);
		gAppName[appNameLen] = 0;

		while (*s == ' ') s++;
		gAppVersion = strdup(s);
	}
	else
	{
		gAppName    = strdup(appTitle);
		gAppVersion = strdup(appVersion);
	}

	s = gAppName;
	appNameLen = 0;
	while (*s)
	{
		if (isalnum(*s))
			gAppName[appNameLen++] = *s++;
		else
			s++;
	}
	gAppName[appNameLen] = 0;

	gAppTitle   = strdup(appTitle);
}

typedef void                    (*HsFunPtr)(void);

extern void hs_free_fun_ptr    (HsFunPtr fp);
extern void getProgArgv(int *argc, char **argv[]);

void osStart(char *appTitle, char *appVersion, int DocumentInterface, OsInitFunc initFunc)
{
	if (!gProgram)
	{
		int margc;
		char **margv;

		parseAppNameVersion(appTitle,appVersion);

		gtk_set_locale();
		getProgArgv(&margc, &margv);
		gProgram = gnome_program_init(gAppName, appVersion, LIBGNOMEUI_MODULE,
                                    margc, margv, GNOME_PARAM_NONE);

		gDocumentInterface = DocumentInterface;
		gFrameWidget = NULL;
		gClientWidget = NULL;
		gMenuBar = NULL;

		if (gDocumentInterface == 2)
			createMDIFrame();
		else
			createSDIFrame();

		char *keyName = strdup("HToolkit.FrameState");
		if (osGetConfigIntKey(keyName, 0) == GDK_WINDOW_STATE_MAXIMIZED)
			gtk_window_maximize(GTK_WINDOW(gFrameWidget));
		free(keyName);
	}

	if (initFunc != NULL) {
		initFunc();
		hs_free_fun_ptr(initFunc);
		initFunc = NULL;
	}

	gtk_main();
	osDestroyAllActions();
	handleProcessDestroy();
	free(gAppTitle);   gAppTitle = NULL;
	free(gAppName);    gAppName = NULL;
	free(gAppVersion); gAppVersion = NULL;
};

void osQuit()
{
	gtk_widget_destroy(gFrameWidget);
	gtk_main_quit();
	gProgram = NULL;
}

gchar *toMnemonicString(const gchar *source)
{
	gchar *dest, *s;

	if (!source)
		return NULL;

	dest = (gchar *) rmalloc(strlen(source)*2+1);
	s = dest;

	while (*source)
	{
		switch (*source)
		{
		case '&':
			source++;
			*(dest++) = (*source == '&') ? *(source++) : '_';
			break;
		case '_':
			*(dest++) = '_';
		default:
			*(dest++) = *(source++);
		}
	}

	*dest = 0;
	return s;
};

gchar *fromMnemonicString(const gchar *source)
{
	gchar *dest = (gchar *) rmalloc(strlen(source)*2+1);
	gchar *s = dest;

	while (*source)
	{
		switch (*source)
		{
		case '_':
			source++;
			*(dest++) = (*source == '_') ? *(source++) : '&';
			break;
		case '&':
			*(dest++) = '&';
		default:
			*(dest++) = *(source++);
		}
	}

	*dest = 0;
	return s;
};
