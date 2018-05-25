#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern void handleWindowDismiss(HsPtr a1);
extern void handleWindowDestroy(HsPtr a1);
extern void handleWindowPaint(HsPtr a1, HsPtr a2, HsInt32 a3, HsInt32 a4, HsInt32 a5, HsInt32 a6);
extern void handleWindowResize(HsPtr a1, HsInt32 a2, HsInt32 a3);
extern void handleWindowScroll(HsPtr a1, HsInt32 a2, HsInt32 a3);
extern void handleWindowMouse(HsPtr a1, HsInt32 a2, HsInt32 a3, HsInt32 a4, HsWord32 a5);
extern void handleWindowKeyboard(HsPtr a1, HsInt32 a2, HsInt32 a3, HsWord32 a4);
extern void handleWindowDeactivate(HsPtr a1);
extern void handleWindowActivate(HsPtr a1);
extern void handleContainerReLayout(HsPtr a1);
extern void handleWindowContextMenu(HsPtr a1, HsInt32 a2, HsInt32 a3, HsWord32 a4);
extern void handleControlCommand(HsPtr a1);
extern void handleTrackBarIncrement(HsPtr a1);
extern void handleTrackBarDecrement(HsPtr a1);
extern void handleMenuDestroy(HsPtr a1);
extern void handleToolDestroy(HsPtr a1);
extern void handleActionCommand(HsPtr a1);
extern void handleActionUpdate(HsPtr a1);
extern void handleActionDestroy(HsPtr a1);
extern void handleTimer(HsPtr a1);
extern void handleTimerDestroy(HsPtr a1);
extern void handleProcessDismiss(void);
extern void handleProcessDestroy(void);
extern void handleIndicatorCommand(HsPtr a1);
extern void handleIndicatorDestroy(HsPtr a1);
#ifdef __cplusplus
}
#endif

