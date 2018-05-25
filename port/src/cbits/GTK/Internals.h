#ifndef ITERNALS_H
#define ITERNALS_H

#include "LayoutContainer.h"

enum ActionType { ACTION_NORMAL, ACTION_CHECK, ACTION_RADIO, ACTION_DROPDOWN };

struct ActionHandle
{
	// chain;
	ActionHandle next_action;
	ActionHandle next_action_in_group;

	// Attributes
	enum ActionType type;        // the action type
	char *title;
	char *short_title;
	char *tooltip;
	BitmapHandle bitmap;
	BOOL enabled : 1;         // TRUE if the action is enabled
	BOOL checked : 1;         // TRUE if the action is checked
	BOOL busy : 1;            // TRUE if we currently update the proxies state

	GtkWidget *menu;

	// Accelerator
	int key;
	unsigned int keyMods;

	GSList *proxies;   // the collection of all proxies (menu items or toolbar buttons) associated to this action
};

extern void menu_item_set_accel(GtkWidget *item, ActionHandle action);

extern BOOL gInKey;
extern int gCurChar;

extern GnomeProgram *gProgram;

extern int gFrameWidth, gFrameHeight;

extern int gDocumentInterface;
extern char *gAppTitle, *gAppName, *gAppVersion, *gWindowName;
extern GtkWidget *gFrameWidget;
extern GtkWidget *gClientWidget;
extern GtkWidget *gMenuBar;

extern gchar *fromMnemonicString(const gchar *source);
extern gchar *toMnemonicString(const gchar *source);

void createMDIFrame();
void createSDIFrame();

extern void rfree(void *ptr);
extern void *rmalloc(unsigned long bytes);

void osForceContainerReLayout(GtkWidget *widget);

WindowHandle create_generic_window();

int CheckVirtualKeyCode (int keycode);
void osDestroyAllActions();

#endif
