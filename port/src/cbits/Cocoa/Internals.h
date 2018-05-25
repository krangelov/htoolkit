#ifndef ITERNALS_H
#define ITERNALS_H

typedef enum { ACTION_NORMAL, ACTION_CHECK, ACTION_RADIO, ACTION_DROPDOWN }  ActionType;

@interface NSActionHandle : NSObject
{
	// chain;
	ActionHandle _next_action;
	ActionHandle _next_action_in_group;

	// Attributes
	ActionType _type;        // the action type
	NSMutableArray* _menuItems;
}

@property (retain,nonatomic) NSString *title;
@property (retain,nonatomic) NSString *short_title;
@property (retain,nonatomic) NSString *tooltip;
@property (retain,nonatomic) BitmapHandle bitmap;
@property (retain,nonatomic) NSMenuItem *menu;
@property (assign,nonatomic) BOOL enabled;
@property (assign,nonatomic) BOOL checked;

- (void)addMenuItem: (NSMenuItem *) item;

@end

extern int gDocumentInterface;
extern char *gAppTitle, *gAppName, *gAppVersion, *gWindowName;
extern NSWindow *gMainWindow;

void osDestroyAllActions();

void osForceContainerReLayout(WindowHandle window);

#endif
