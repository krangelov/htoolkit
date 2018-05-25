#include "Window.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include <gdk/gdkkeysyms.h>

#define EDGE_SIZE 3
#define TITLE_SIZE 13

char *gWindowName = NULL;

static void getWindowClipRect(GtkWidget *window, GdkRegion *region)
{
	GList *children = gtk_container_get_children(GTK_CONTAINER(GTK_BIN(window)->child));

	while (children)
	{
		GdkRectangle rectangle;
		GtkWidget *widget = (GtkWidget *) children->data;

		rectangle.x = widget->allocation.x;
		rectangle.y = widget->allocation.y;
		rectangle.width = widget->allocation.width;
		rectangle.height = widget->allocation.height;
		gdk_region_subtract(region, gdk_region_rectangle (&rectangle));

		children = g_list_remove(children, widget);
	}
}

void osInvalidateWindow(WindowHandle window)
{
	gtk_widget_queue_draw(window);
};

void osInvalidateWindowRect(WindowHandle window, int left, int top, int right, int bottom)
{
	gtk_widget_queue_draw_area(window,left,top,right-left,bottom-top);
}

static gboolean window_expose_handler (GtkWidget *widget, GdkEventExpose *event, gpointer user_data)
{
	CanvasHandle canvas;
	GtkWidget *window = (GtkWidget *) user_data;
	GtkWidget *layout = GTK_BIN(window)->child;
	GdkGC *gc;

	gc = gdk_gc_new(event->window);
	gdk_gc_set_foreground(gc, &(gtk_widget_get_style(layout)->bg[GTK_STATE_NORMAL]));
	gdk_gc_set_clip_region(gc, event->region);

	gdk_draw_rectangle(event->window, gc, TRUE,
			                   event->area.x, event->area.y, event->area.width, event->area.height);

	gdk_gc_destroy(gc);

	canvas = rmalloc(sizeof(*canvas));
	memset(canvas, 0, sizeof(*canvas));
	canvas->drawable = GDK_DRAWABLE(event->window);
	canvas->layout = gtk_widget_create_pango_layout(layout, NULL);
	canvas->region = gdk_region_copy(event->region);
	canvas->buffered = (GTK_WIDGET_FLAGS(widget) & GTK_DOUBLE_BUFFERED) ? 2 : 0;
	getWindowClipRect(window, canvas->region);

	handleWindowPaint(window,canvas,
				event->area.x, event->area.y, event->area.x+event->area.width, event->area.y+event->area.height);

	if (canvas->region) gdk_region_destroy(canvas->region);
	g_object_unref(canvas->layout);
	
	return gtk_false();
}

static BOOL bInDragMode = FALSE;

static gboolean window_button_press_handler(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	GtkWidget *window = (GtkWidget *) user_data;

	int modifiers = 0;
	if (event->state & GDK_SHIFT_MASK) modifiers |= shiftBIT;
	if (event->state & GDK_CONTROL_MASK) modifiers |= ctrlBIT;
	if (event->state & GDK_MOD1_MASK) modifiers |= altBIT;

	switch (event->type)
	{
		case GDK_BUTTON_PRESS:
			switch (event->button)
			{
				case 1:
					bInDragMode = TRUE;
					handleWindowMouse(window,evMouseLeftDown,event->x,event->y,modifiers);
					break;
				case 3:
					handleWindowMouse(window,evMouseRightDown,event->x,event->y,modifiers);
					handleWindowContextMenu(window,event->x,event->y,modifiers);
					break;
			}
			break;
		case GDK_2BUTTON_PRESS:
			if (event->button == 1)
				handleWindowMouse(window,evMouseDoubleClick,event->x,event->y,modifiers);
			break;
		default:
			break;
	}

	return gtk_true();
}

static gboolean window_button_release_handler(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	GtkWidget *window = (GtkWidget *) user_data;

	int modifiers = 0;
	if (event->state & GDK_SHIFT_MASK) modifiers |= shiftBIT;
	if (event->state & GDK_CONTROL_MASK) modifiers |= ctrlBIT;
	if (event->state & GDK_MOD1_MASK) modifiers |= altBIT;

	switch (event->button)
	{
		case 1:
			bInDragMode = FALSE;
			handleWindowMouse(window,evMouseLeftUp,event->x,event->y,modifiers);
			break;
		case 3:
			handleWindowMouse(window,evMouseRightUp,event->x,event->y,modifiers);
			break;
	}

	return gtk_true();
}

static gboolean window_motion_notify_handler(GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
	GtkWidget *window = (GtkWidget *) user_data;

	int modifiers = 0;
	if (event->state & GDK_SHIFT_MASK) modifiers |= shiftBIT;
	if (event->state & GDK_CONTROL_MASK) modifiers |= ctrlBIT;
	if (event->state & GDK_MOD1_MASK) modifiers |= altBIT;

	if (bInDragMode)
		handleWindowMouse(window,evMouseDrag,event->x,event->y,modifiers);
	else
		handleWindowMouse(window,evMouseMove,event->x,event->y,modifiers);

	return gtk_true();
};

static void frame_adjustment_value_changed_handler (GtkAdjustment *adjustment, gpointer user_data)
{
	int x, y;
	GtkWidget *window = (GtkWidget *) user_data;
	GtkScrolledWindow *sw = GTK_SCROLLED_WINDOW(window);

	x = gtk_scrolled_window_get_hadjustment (sw)->value;
	y = gtk_scrolled_window_get_vadjustment (sw)->value;

	handleWindowScroll(window,x,y);
};

static gboolean window_delete_handler(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
	handleWindowDismiss(widget);
	return gtk_true();
}

WindowHandle create_generic_window()
{
	GtkWidget *layout, *sw;

	/* Create a Scrolled Window */
	sw = gtk_scrolled_window_new (NULL, NULL);
	gtk_signal_connect (GTK_OBJECT(sw), "destroy",
		GTK_SIGNAL_FUNC(handleWindowDestroy),
		NULL);
	gtk_signal_connect (GTK_OBJECT (sw), "delete-event",
		GTK_SIGNAL_FUNC(window_delete_handler),
		NULL);

	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
					  GTK_POLICY_AUTOMATIC,
					  GTK_POLICY_AUTOMATIC);

	/* Create a Fixed Container */
	layout = port_layout_new(NULL,NULL);
	gtk_container_add(GTK_CONTAINER(sw), layout);

	gtk_widget_show(layout);

	/* Signals */
	gtk_signal_connect (GTK_OBJECT(layout), "expose-event",
			GTK_SIGNAL_FUNC(window_expose_handler),
			sw);
	gtk_signal_connect (GTK_OBJECT(layout), "button-press-event",
			GTK_SIGNAL_FUNC(window_button_press_handler),
			sw);
	gtk_signal_connect (GTK_OBJECT(layout), "button-release-event",
			GTK_SIGNAL_FUNC(window_button_release_handler),
			sw);
	gtk_signal_connect (GTK_OBJECT(layout), "motion_notify_event",
			GTK_SIGNAL_FUNC(window_motion_notify_handler),
			sw);
	gtk_signal_connect (GTK_OBJECT (port_layout_get_hadjustment (PORT_LAYOUT(layout))), "value-changed",
			GTK_SIGNAL_FUNC(frame_adjustment_value_changed_handler),
			sw);
	gtk_signal_connect (GTK_OBJECT (port_layout_get_vadjustment (PORT_LAYOUT(layout))), "value-changed",
			GTK_SIGNAL_FUNC(frame_adjustment_value_changed_handler),
			sw);
	gtk_widget_set_events(GTK_WIDGET(layout), GDK_BUTTON_PRESS_MASK | 
	                                          GDK_BUTTON_RELEASE_MASK |
	                                          GDK_BUTTON_MOTION_MASK);

	return sw;
}

WindowHandle osCreateWindow()
{
	GtkWidget *sw, *layout;

	if (gDocumentInterface == 1 && gClientWidget != NULL)
		return NULL;

	sw = create_generic_window();
	layout = GTK_BIN(sw)->child;

	if (gDocumentInterface == 2)
		gtk_notebook_append_page(GTK_NOTEBOOK(gClientWidget),sw,gtk_label_new(""));
	else
	{
		gClientWidget = sw;
		gnome_app_set_contents(GNOME_APP(gFrameWidget),sw);
		gtk_widget_hide(sw);
	}

	return sw;
}

static gboolean dialog_key_press_handler(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
	int c;
	int modifiers = 0;

	widget = GTK_BIN(widget)->child;

	if (event->state & GDK_SHIFT_MASK) modifiers |= shiftBIT;
	if (event->state & GDK_CONTROL_MASK) modifiers |= ctrlBIT;
	if (event->state & GDK_MOD1_MASK) modifiers |= altBIT;

	if (event->length > 0)
	{
		c = event->string[0];
		if (modifiers & altBIT) c += 256;
	}
	else
		c = CheckVirtualKeyCode(event->keyval);

	if (!c) return gtk_false();

	if (gInKey)

	{
		if (gCurChar == c)
			handleWindowKeyboard(widget, evKeyStillDown, gCurChar, modifiers);
		else
		{
			handleWindowKeyboard(widget, evKeyUp, gCurChar, modifiers);
			gCurChar = c;
			handleWindowKeyboard(widget, evKeyDown, gCurChar, modifiers);
		}
	}
	else
	{
		gCurChar = c;
		handleWindowKeyboard(widget, evKeyDown, gCurChar, modifiers);
		gInKey = TRUE;
	}

	return gtk_false();
};

static gboolean dialog_key_release_handler(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
	if (gInKey)
	{
		int modifiers = 0;

		if (event->state & GDK_SHIFT_MASK) modifiers |= shiftBIT;
		if (event->state & GDK_CONTROL_MASK) modifiers |= ctrlBIT;
		if (event->state & GDK_MOD1_MASK) modifiers |= altBIT;

		handleWindowKeyboard(widget, evKeyUp, gCurChar, modifiers);
		gInKey = FALSE;
		gCurChar = 0;
	}

	return gtk_false();
};

static void dialog_focus_in_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	handleWindowActivate(widget);
}

static void dialog_focus_out_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	if (gInKey)
	{
		handleWindowKeyboard(widget, evKeyUp, gCurChar, 0);

		gInKey = FALSE;
		gCurChar = 0;
	}

	handleWindowDeactivate(widget);
}



WindowHandle osCreateDialog(WindowHandle parent)
{
	GtkWidget *layout, *frame;

	if (parent == NULL)
		parent = gFrameWidget;
	else
		parent = gtk_widget_get_toplevel(parent);

	frame = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_transient_for(GTK_WINDOW(frame), GTK_WINDOW(parent));
	gtk_window_set_destroy_with_parent(GTK_WINDOW(frame), gtk_true());

	gtk_signal_connect (GTK_OBJECT(frame), "key-press-event",
		GTK_SIGNAL_FUNC(dialog_key_press_handler),
		NULL);
	gtk_signal_connect (GTK_OBJECT(frame), "key-release-event",
		GTK_SIGNAL_FUNC(dialog_key_release_handler),
		NULL);
	gtk_signal_connect (GTK_OBJECT(frame), "focus-in-event",
		GTK_SIGNAL_FUNC(dialog_focus_in_handler),
		NULL);
	gtk_signal_connect (GTK_OBJECT(frame), "focus-out-event",
		GTK_SIGNAL_FUNC(dialog_focus_out_handler),
		NULL);
	gtk_signal_connect (GTK_OBJECT(frame), "destroy",
		GTK_SIGNAL_FUNC(handleWindowDestroy),
		NULL);
	gtk_signal_connect (GTK_OBJECT(frame), "delete-event",
		GTK_SIGNAL_FUNC(window_delete_handler),
		NULL);

	layout = port_layout_new(NULL,NULL);
	gtk_container_add(GTK_CONTAINER(frame), layout);
	gtk_widget_show(layout);

	/* Signals */
	gtk_signal_connect (GTK_OBJECT(layout), "expose-event",
			GTK_SIGNAL_FUNC(window_expose_handler),
			frame);
	gtk_signal_connect (GTK_OBJECT(layout), "button-press-event",
			GTK_SIGNAL_FUNC(window_button_press_handler),
			frame);
	gtk_signal_connect (GTK_OBJECT(layout), "button-release-event",
			GTK_SIGNAL_FUNC(window_button_release_handler),
			frame);
	gtk_signal_connect (GTK_OBJECT(layout), "motion_notify_event",
			GTK_SIGNAL_FUNC(window_motion_notify_handler),
			frame);

	return frame;
}

WindowHandle osCreateCompoundControl(WindowHandle form)
{
	GtkWidget *sw;

	sw = create_generic_window();
	port_layout_put(PORT_LAYOUT(GTK_BIN(form)->child), sw);

	gtk_widget_show_all(sw);

	return sw;
}

void osGetCompoundControlReqSize(WindowHandle listbox, int *res)
{
	res[0] = 10;
	res[1] = 10;
}

void osSetWindowColor(WindowHandle window, int foreColor, int backColor, int hatchStyle, BitmapHandle hatchBitmap)
{
	GdkColor bcolor;
	GdkColor fcolor;

	GtkWidget *layout = GTK_BIN(window)->child;
	GList *childrens = gtk_container_get_children(GTK_CONTAINER(layout));

	bcolor.pixel = 0;
	bcolor.red   = ((backColor      ) & 0xFF)*257;
	bcolor.green = ((backColor >>  8) & 0xFF)*257;
	bcolor.blue  = ((backColor >> 16) & 0xFF)*257;

	fcolor.pixel = 0;
	fcolor.red   = ((foreColor      ) & 0xFF)*257;
	fcolor.green = ((foreColor >>  8) & 0xFF)*257;
	fcolor.blue  = ((foreColor >> 16) & 0xFF)*257;

	gtk_widget_modify_bg(layout,  GTK_STATE_NORMAL, &bcolor);

	while (childrens)
	{
		GtkWidget *widget = (GtkWidget *) childrens->data;

		if (GTK_IS_LABEL(widget))
		{
			gtk_widget_modify_fg(widget,  GTK_STATE_NORMAL, &fcolor);
			gtk_widget_modify_bg(widget,  GTK_STATE_NORMAL, &bcolor);
		}
		else
			if (GTK_IS_TOGGLE_BUTTON(widget))
			{
				gtk_widget_modify_bg(widget,  GTK_STATE_PRELIGHT, &bcolor);
				gtk_widget_modify_fg(GTK_BIN(widget)->child,  GTK_STATE_NORMAL, &fcolor);
				gtk_widget_modify_fg(GTK_BIN(widget)->child,  GTK_STATE_ACTIVE, &fcolor);
				gtk_widget_modify_fg(GTK_BIN(widget)->child,  GTK_STATE_PRELIGHT, &fcolor);
				gtk_widget_modify_fg(GTK_BIN(widget)->child,  GTK_STATE_SELECTED, &fcolor);
			}

		childrens = childrens->next;
	}
}

char *osGetWindowTitle(WindowHandle window)
{
	GtkWidget *toplevel = gtk_widget_get_toplevel(window);

	if (toplevel == gFrameWidget)
	{
		if (gDocumentInterface == 2)
		{
			GtkWidget *label;

			label = gtk_notebook_get_tab_label(GTK_NOTEBOOK(gClientWidget),window);
			gWindowName = (char*) gtk_label_get_text(GTK_LABEL(label));
		}


		return strdup(gWindowName);
	}
	else
	{
		return strdup(gtk_window_get_title(GTK_WINDOW(toplevel)));
	}
};

void osSetWindowTitle(WindowHandle window, char *title)
{
	GtkWidget *toplevel = gtk_widget_get_toplevel(window);

	if (toplevel == gFrameWidget)
	{
		if (gDocumentInterface == 2)
		{
			GtkWidget *label;

			label = gtk_notebook_get_tab_label(GTK_NOTEBOOK(gClientWidget),window);
			gtk_label_set_text(GTK_LABEL(label), title);

			gtk_notebook_set_menu_label_text(GTK_NOTEBOOK(gClientWidget),window,title);
		}
		else
		{
			gWindowName = strdup(title);

			if (gAppName && *gAppName)
			{
				if (gWindowName && *gWindowName)
				{
					title = rmalloc(strlen(gAppName)+strlen(gWindowName)+6);

					if (title)
					{
						strcpy(title, gAppName);
						strcat(title, " - [");
						strcat(title, gWindowName);
						strcat(title, "]");
						gtk_window_set_title(GTK_WINDOW(gFrameWidget), title);
					}

					rfree(title);
				}
				else
					gtk_window_set_title(GTK_WINDOW(gFrameWidget), gAppName);
			}
			else
				gtk_window_set_title(GTK_WINDOW(gFrameWidget), gWindowName);
		}
	}
	else
	{
		gtk_window_set_title(GTK_WINDOW(toplevel), title);
	}
};

// The implementation for osGetWindowViewSize and osSetWindowViewSize functions
// is inspired from implementation for DoGetClientSize and DoSetClientSize functions from
// wxWindows. The trouble here is the asume that the window frame borders are
// with constant sizes (see EDGE_SIZE and TITLE_SIZE constants). This is not true at all.
void osGetWindowViewSize(WindowHandle window, int *res)
{
	GtkWidget *toplevel = gtk_widget_get_toplevel(window);

	if (toplevel != gFrameWidget || gDocumentInterface == 1)
	{
		GtkRequisition req;

		res[0] = toplevel->allocation.width-4;
		res[1] = toplevel->allocation.height-4;

		if (GNOME_IS_APP(toplevel))
		{
			GtkWidget *menubar = GNOME_APP(toplevel)->menubar;

			if (menubar != NULL)
			{
				req.width = 2;
				req.height = 2;
				(* GTK_WIDGET_CLASS( GTK_OBJECT_GET_CLASS(menubar) )->size_request )
				                                                                                             (menubar, &req);
				res[1] -= req.height;
			}
		}
	}
	else
	{
		res[0] = window->allocation.width-4;
		res[1] = window->allocation.height-4;
	}
}

void osSetWindowViewSize(WindowHandle window, int w, int h)
{
	GtkWidget *toplevel = gtk_widget_get_toplevel(window);
	if (toplevel != gFrameWidget || gDocumentInterface == 1)
	{
		GtkRequisition req;

		if (GTK_IS_SCROLLED_WINDOW(window))
		{
			GtkScrolledWindow *scroll_window = GTK_SCROLLED_WINDOW(window);
			GtkScrolledWindowClass *scroll_class = GTK_SCROLLED_WINDOW_CLASS(GTK_OBJECT_GET_CLASS(window));

			if (scroll_window->vscrollbar_visible)
			{
				req.width = 2;
				req.height = 2;
				(* GTK_WIDGET_CLASS( GTK_OBJECT_GET_CLASS(scroll_window->vscrollbar) )->size_request )
			                                                                                                    (scroll_window->vscrollbar, &req );
				w += req.width + scroll_class->scrollbar_spacing;
			}

			if (scroll_window->hscrollbar_visible)
			{
				req.width = 2;
				req.height = 2;
				(* GTK_WIDGET_CLASS( GTK_OBJECT_GET_CLASS(scroll_window->hscrollbar) )->size_request )
			                                                                                                     (scroll_window->hscrollbar, &req );
				h += req.height + scroll_class->scrollbar_spacing;
			}
		}

		if (GNOME_IS_APP(toplevel))
		{
			GtkWidget *menubar = GNOME_APP(toplevel)->menubar;

			if (menubar != NULL)
			{
				req.width = 2;
				req.height = 2;
				(* GTK_WIDGET_CLASS( GTK_OBJECT_GET_CLASS(menubar) )->size_request )
				                                                                                             (menubar, &req);
				h += req.height;
			}
		}

		w += EDGE_SIZE*2;
		h += EDGE_SIZE*2 + TITLE_SIZE;

		gFrameWidth  = w;
		gFrameHeight = h;
		gtk_window_resize(GTK_WINDOW(toplevel), w, h);
	}
}

void osSetWindowDomainSize(WindowHandle window, int cx, int cy)
{
	port_layout_set_domain(PORT_LAYOUT(GTK_BIN(window)->child), cx, cy);
}

void osSetWindowScrollOrigin(WindowHandle window, int x, int y)
{
	if (GTK_IS_SCROLLED_WINDOW(window))
	{
		gtk_adjustment_set_value(gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW(window)), x);
		gtk_adjustment_set_value(gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW(window)), y);
	}
}

void osGetWindowScrollOrigin(WindowHandle window, int *res)
{
	if (GTK_IS_SCROLLED_WINDOW(window))
	{
		res[0] = gtk_adjustment_get_value(gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW(window)));
		res[1] = gtk_adjustment_get_value(gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW(window)));
	}
	else
	{
		res[0] = 0;
		res[1] = 0;
	}
}

void osSetWindowLineSize(WindowHandle window, int cx, int cy)
{
	if (GTK_IS_SCROLLED_WINDOW(window))
	{
		GtkAdjustment *adj;
		GtkScrolledWindow *sw = GTK_SCROLLED_WINDOW(window);
		adj = gtk_scrolled_window_get_hadjustment(sw); adj->step_increment = cx;  gtk_adjustment_changed(adj);
		adj = gtk_scrolled_window_get_vadjustment(sw); adj->step_increment = cy;  gtk_adjustment_changed(adj);
	}
}

void osGetWindowLineSize(WindowHandle window, int *res)
{
	if (GTK_IS_SCROLLED_WINDOW(window))
	{
		GtkScrolledWindow *sw = GTK_SCROLLED_WINDOW(window);
		res[0] = gtk_scrolled_window_get_hadjustment(sw)->step_increment;
		res[1] = gtk_scrolled_window_get_vadjustment(sw)->step_increment;
	}
}

void osSetWindowPageSize(WindowHandle window, int cx, int cy)
{
	if (GTK_IS_SCROLLED_WINDOW(window))
	{
		GtkAdjustment *adj;
		GtkScrolledWindow *sw = GTK_SCROLLED_WINDOW(window);
		adj = gtk_scrolled_window_get_hadjustment(sw); adj->page_increment = cx;  gtk_adjustment_changed(adj);
		adj = gtk_scrolled_window_get_vadjustment(sw); adj->page_increment = cy;  gtk_adjustment_changed(adj);
	}
}


void osGetWindowPageSize(WindowHandle window, int *res)
{
	if (GTK_IS_SCROLLED_WINDOW(window))
	{
		GtkScrolledWindow *sw = GTK_SCROLLED_WINDOW(window);
		res[0] = gtk_scrolled_window_get_hadjustment(sw)->page_increment;
		res[1] = gtk_scrolled_window_get_vadjustment(sw)->page_increment;
	}
}

void osSetWindowVisible(WindowHandle window, BOOL visible)
{
	if (visible)
		gtk_widget_show(window);
	else
		gtk_widget_hide(window);
};

BOOL osGetWindowVisible(WindowHandle window)
{
	return GTK_WIDGET_VISIBLE(window);
};

void osRunDialog(WindowHandle window)
{
	gtk_widget_show(window);

	gtk_window_set_modal(GTK_WINDOW(window), TRUE);
	gtk_signal_connect (GTK_OBJECT(window), "destroy",
		GTK_SIGNAL_FUNC(gtk_main_quit),
		NULL);

	gtk_main();
}

BOOL osDismissWindow(WindowHandle window)
{
	gtk_signal_connect (GTK_OBJECT(window), "destroy",
			GTK_SIGNAL_FUNC(gtk_widget_destroyed),
			&window);
	handleWindowDismiss(window);
	return (window == NULL);
}

void osDestroyWindow(WindowHandle window)
{
	gtk_widget_destroy(window);
}

void osSetWindowEnabled(WindowHandle window, BOOL enabled)
{
	gtk_widget_set_sensitive(window,enabled);
}

BOOL osGetWindowEnabled(WindowHandle window)
{
	return GTK_WIDGET_SENSITIVE(window);
}

CanvasHandle osGetWindowCanvas(WindowHandle window)
{
	GdkRectangle rectangle;
	CanvasHandle canvas;
	GtkWidget *layout;

	layout = GTK_BIN(window)->child;

	if (GTK_IS_SCROLLED_WINDOW(window))
	{
		int scrollbar_spacing;
		GtkRequisition child_requisition, hscrollbar_requisition, vscrollbar_requisition;

		rectangle.x = gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW(window))->value;
		rectangle.y = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW(window))->value;
		rectangle.width = window->allocation.width;
		rectangle.height = window->allocation.height;

		gtk_widget_size_request(layout, &child_requisition);

		gtk_widget_size_request(GTK_SCROLLED_WINDOW(window)->hscrollbar, &hscrollbar_requisition);
		gtk_widget_size_request(GTK_SCROLLED_WINDOW(window)->vscrollbar, &vscrollbar_requisition);

		scrollbar_spacing = GTK_SCROLLED_WINDOW_GET_CLASS(window)->scrollbar_spacing;
		if (scrollbar_spacing < 0) scrollbar_spacing = 3;

		if (rectangle.width  < child_requisition.width ) rectangle.width -= scrollbar_spacing + vscrollbar_requisition.width;
		if (rectangle.height < child_requisition.height) rectangle.height -= scrollbar_spacing + hscrollbar_requisition.height;
	}
	else
	{
		rectangle.x = 0;
		rectangle.y = 0;
		rectangle.width = window->allocation.width;
		rectangle.height = window->allocation.height;
	}

	rectangle.width  -= 2*GTK_CONTAINER(window)->border_width;
	rectangle.height -= 2*GTK_CONTAINER(window)->border_width;

	canvas = rmalloc(sizeof(*canvas));
	memset(canvas, 0, sizeof(*canvas));
	canvas->drawable = GDK_DRAWABLE(PORT_LAYOUT(layout)->bin_window);
	canvas->layout = gtk_widget_create_pango_layout(layout, NULL);
	canvas->region = gdk_region_rectangle(&rectangle);
	canvas->buffered = 0;

	gdk_window_ref(canvas->drawable);
	getWindowClipRect(window, canvas->region);

	return canvas;
}	/* osGetWindowCanvas */

void osReleaseWindowCanvas(WindowHandle widget, CanvasHandle canvas)
{
	gdk_window_unref(GDK_WINDOW(canvas->drawable));
	if (canvas->region) gdk_region_destroy(canvas->region);
	g_object_unref(canvas->layout);
	rfree(canvas);
}	/* osReleaseWindowCanvas */

void osMoveResizeControl(WindowHandle ctrl, int x, int y, int w, int h)
{
	GtkAllocation child_allocation;

	child_allocation.x = x;
	child_allocation.y = y;
	child_allocation.width = w;
	child_allocation.height = h;
	gtk_widget_size_allocate (ctrl, &child_allocation);
}

void osGetControlRect(WindowHandle ctrl, int *res)
{
	res[0] = ctrl->allocation.x;
	res[1] = ctrl->allocation.y;
	res[2] = ctrl->allocation.width;
	res[3] = ctrl->allocation.height;
}

void osSetControlEnabled(WindowHandle ctrl, BOOL enabled)
{
	gtk_widget_set_sensitive(ctrl,enabled);
}

BOOL osGetControlEnabled(WindowHandle ctrl)
{
	return GTK_WIDGET_SENSITIVE(ctrl);
}

void osSetControlVisible(WindowHandle ctrl, BOOL visible)
{
	if (visible)
		gtk_widget_show(ctrl);
	else
		gtk_widget_hide(ctrl);
}

BOOL osGetControlVisible(WindowHandle ctrl)
{
	return GTK_WIDGET_VISIBLE(ctrl);
}

static GtkTooltips *gTooltips;

void osSetControlTip(WindowHandle ctrl, char *text)
{
	if (!gTooltips)
		gTooltips = gtk_tooltips_new();

	gtk_tooltips_set_tip(gTooltips, ctrl, text, NULL);
}

char *osGetControlTip(WindowHandle ctrl)
{
	GtkTooltipsData *tips_data;
	if (!gTooltips)
		return NULL;

	tips_data = gtk_tooltips_data_get(ctrl);
	return strdup(tips_data->tip_text);
}

void osSetWindowPosition(WindowHandle window, int position, int x0, int y0, int x1, int y1)
{
	GtkWidget *toplevel = gtk_widget_get_toplevel(window);

	if (toplevel != gFrameWidget || gDocumentInterface == 1)
	{
		switch (position)
		{
		case 1: gtk_window_set_position(GTK_WINDOW(toplevel), GTK_WIN_POS_CENTER_ALWAYS); break;
		case 2: gtk_window_set_position(GTK_WINDOW(toplevel), GTK_WIN_POS_CENTER_ON_PARENT); break;
		case 3: gtk_window_set_position(GTK_WINDOW(toplevel), GTK_WIN_POS_MOUSE); break;
		default:
			gtk_window_move(GTK_WINDOW(toplevel), x0, y0);
		}

		gFrameWidth  = abs(x1 - x0);
		gFrameHeight = abs(y1 - y0);
		gtk_window_resize(GTK_WINDOW(toplevel), gFrameWidth, gFrameHeight);
	}
}

void osGetWindowRect(WindowHandle window, int *res)
{
	GtkWidget *toplevel = gtk_widget_get_toplevel(window);

	if (toplevel == gFrameWidget && gDocumentInterface == 2)
	{
		res[0] = 0;
		res[1] = 0;
		res[2] = window->allocation.width;
		res[3] = window->allocation.height;
	}
	else
	{
  		int x = 0;
		int y = 0;

		gtk_window_get_position(GTK_WINDOW(toplevel), &x, &y);

		res[0] = x;
		res[1] = y;
		res[2] = x+gFrameWidth;
		res[3] = y+gFrameHeight;
	}
}

void osSetWindowResizeable(WindowHandle window, int resizeable)
{
	GtkWidget *toplevel = gtk_widget_get_toplevel(window);

	if (toplevel != gFrameWidget || gDocumentInterface == 1)
	{
		if (resizeable)
			gtk_window_set_geometry_hints(GTK_WINDOW(toplevel), toplevel, NULL, 0);
		else
		{
			GdkGeometry geometry;

			if (toplevel != gFrameWidget)
			{
				geometry.min_width  = toplevel->allocation.width;
				geometry.min_height = toplevel->allocation.height;
				geometry.max_width  = toplevel->allocation.width;
				geometry.max_height = toplevel->allocation.height;
			}
			else
			{
				geometry.min_width  = gFrameWidth;
				geometry.min_height = gFrameHeight;
				geometry.max_width  = gFrameWidth;
				geometry.max_height = gFrameHeight;
			}

			gtk_window_set_geometry_hints(GTK_WINDOW(toplevel), toplevel, &geometry,
		                                                                GDK_HINT_MIN_SIZE | GDK_HINT_MAX_SIZE);
		}
	}
}

void osForceContainerReLayout(GtkWidget *widget)
{
	while (widget)
	{
		widget = gtk_widget_get_parent(gtk_widget_get_parent(widget));

		if (!GTK_IS_FRAME(widget) && !GTK_IS_NOTEBOOK(gtk_widget_get_parent(widget)))
		{
			handleContainerReLayout(widget);
			break;
		}
	}
}

void osReLayoutContainer(WindowHandle window)
{
	if (GTK_IS_FRAME(window) || GTK_IS_NOTEBOOK(gtk_widget_get_parent(window)))
		osForceContainerReLayout(window);

	handleContainerReLayout(window);
}

void osSetDialogMinSize(WindowHandle dialog, int w, int h)
{
	if (gtk_widget_get_toplevel(dialog) != gFrameWidget)
	{
		PORT_LAYOUT(GTK_BIN(dialog)->child)->requisition.width = w;
		PORT_LAYOUT(GTK_BIN(dialog)->child)->requisition.height = h;
	}
}
