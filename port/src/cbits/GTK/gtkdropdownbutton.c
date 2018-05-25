/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "gtkdropdownbutton.h"

#define _(x) x

#define CHILD_LEFT_SPACING        1
#define CHILD_RIGHT_SPACING       1
#define CHILD_TOP_SPACING         1
#define CHILD_BOTTOM_SPACING      1

typedef struct _GtkDropDownButtonProps GtkDropDownButtonProps;

struct _GtkDropDownButtonProps
{
  gboolean interior_focus;
  GtkRequisition indicator_size;
  GtkBorder indicator_spacing;
  gint focus_width;
  gint focus_pad;
};

static const GtkDropDownButtonProps default_props = {
  TRUE,
  { 7, 13 },
  { 1, 1, 1, 1 },		/* Left, right, top, bottom */
  1,
  0
};

static void gtk_drop_down_button_class_init      (GtkDropDownButtonClass *klass);
static void gtk_drop_down_button_init            (GtkDropDownButton      *button);
static void gtk_drop_down_button_destroy         (GtkObject          *object);
static void gtk_drop_down_button_set_property    (GObject            *object,
					     guint               prop_id,
					     const GValue       *value,
					     GParamSpec         *pspec);
static void gtk_drop_down_button_get_property    (GObject            *object,
					     guint               prop_id,
					     GValue             *value,
					     GParamSpec         *pspec);
static void gtk_drop_down_button_size_request    (GtkWidget          *widget,
					     GtkRequisition     *requisition);
static void gtk_drop_down_button_size_allocate   (GtkWidget          *widget,
					     GtkAllocation      *allocation);
static void gtk_drop_down_button_paint           (GtkWidget          *widget,
					     GdkRectangle       *area);
static gint gtk_drop_down_button_expose          (GtkWidget          *widget,
					     GdkEventExpose     *event);
static gint gtk_drop_down_button_button_press    (GtkWidget          *widget,
					     GdkEventButton     *event);
static gint gtk_drop_down_button_key_press	    (GtkWidget          *widget,
					     GdkEventKey        *event);
static void gtk_drop_down_button_position        (GtkMenu            *menu,
					     gint               *x,
					     gint               *y,
					     gint               *scroll_offet,
					     gpointer            user_data);
static void gtk_drop_down_button_show_all        (GtkWidget          *widget);
static void gtk_drop_down_button_hide_all        (GtkWidget          *widget);
static gboolean gtk_drop_down_button_mnemonic_activate (GtkWidget    *widget,
						   gboolean      group_cycling);

enum
{
  PROP_0,
  PROP_MENU,
  LAST_PROP
};

static const GtkBorder default_default_border = { 1, 1, 1, 1 };
static const GtkBorder default_default_outside_border = { 0, 0, 0, 0 };

static void
gtk_button_get_props (GtkButton *button,
		      GtkBorder *default_border,
		      GtkBorder *default_outside_border,
		      gboolean  *interior_focus)
{
  GtkWidget *widget =  GTK_WIDGET (button);
  GtkBorder *tmp_border;

  if (default_border)
    {
      gtk_widget_style_get (widget, "default_border", &tmp_border, NULL);

      if (tmp_border)
	{
	  *default_border = *tmp_border;
	  g_free (tmp_border);
	}
      else
	*default_border = default_default_border;
    }

  if (default_outside_border)
    {
      gtk_widget_style_get (widget, "default_outside_border", &tmp_border, NULL);

      if (tmp_border)
	{
	  *default_outside_border = *tmp_border;
	  g_free (tmp_border);
	}
      else
	*default_outside_border = default_default_outside_border;
    }

  if (interior_focus)
    gtk_widget_style_get (widget, "interior_focus", interior_focus, NULL);
}

void _gtk_dropdownbutton_paint (GtkButton    *button,
		   GdkRectangle *area,
		   GtkStateType  state_type,
		   GtkShadowType shadow_type,
		   const gchar  *main_detail,
		   const gchar  *default_detail)
{
  GtkWidget *widget;
  gint width, height;
  gint x, y;
  gint border_width;
  GtkBorder default_border;
  GtkBorder default_outside_border;
  gboolean interior_focus;
  gint focus_width;
  gint focus_pad;

  if (GTK_WIDGET_DRAWABLE (button))
    {
      widget = GTK_WIDGET (button);
      border_width = GTK_CONTAINER (widget)->border_width;

      gtk_button_get_props (button, &default_border, &default_outside_border, &interior_focus);
      gtk_widget_style_get (GTK_WIDGET (widget),
			    "focus-line-width", &focus_width,
			    "focus-padding", &focus_pad,
			    NULL);

      x = widget->allocation.x + border_width;
      y = widget->allocation.y + border_width;
      width = widget->allocation.width - border_width * 2;
      height = widget->allocation.height - border_width * 2;

      if (GTK_WIDGET_HAS_DEFAULT (widget) &&
	  GTK_BUTTON (widget)->relief == GTK_RELIEF_NORMAL)
	{
	  gtk_paint_box (widget->style, widget->window,
			 GTK_STATE_NORMAL, GTK_SHADOW_IN,
			 area, widget, "buttondefault",
			 x, y, width, height);

	  x += default_border.left;
	  y += default_border.top;
	  width -= default_border.left + default_border.right;
	  height -= default_border.top + default_border.bottom;
	}
      else if (GTK_WIDGET_CAN_DEFAULT (widget))
	{
	  x += default_outside_border.left;
	  y += default_outside_border.top;
	  width -= default_outside_border.left + default_outside_border.right;
	  height -= default_outside_border.top + default_outside_border.bottom;
	}

      if (!interior_focus && GTK_WIDGET_HAS_FOCUS (widget))
	{
	  x += focus_width + focus_pad;
	  y += focus_width + focus_pad;
	  width -= 2 * (focus_width + focus_pad);
	  height -= 2 * (focus_width + focus_pad);
	}

      if (button->relief != GTK_RELIEF_NONE || button->depressed ||
	  GTK_WIDGET_STATE(widget) == GTK_STATE_PRELIGHT)
	gtk_paint_box (widget->style, widget->window,
		       state_type,
		       shadow_type, area, widget, "button",
		       x, y, width, height);

      if (GTK_WIDGET_HAS_FOCUS (widget))
	{
	  if (interior_focus)
	    {
	      x += widget->style->xthickness + focus_pad;
	      y += widget->style->ythickness + focus_pad;
	      width -= 2 * (widget->style->xthickness + focus_pad);
	      height -=  2 * (widget->style->ythickness + focus_pad);
	    }
	  else
	    {
	      x -= focus_width + focus_pad;
	      y -= focus_width + focus_pad;
	      width += 2 * (focus_width + focus_pad);
	      height += 2 * (focus_width + focus_pad);
	    }

	  gtk_paint_focus (widget->style, widget->window, GTK_WIDGET_STATE (widget),
			   area, widget, "button",
			   x, y, width, height);
	}
    }
}


static GtkButtonClass *parent_class = NULL;

GType
gtk_drop_down_button_get_type (void)
{
  static GType option_menu_type = 0;

  if (!option_menu_type)
    {
      static const GTypeInfo option_menu_info =
      {
	sizeof (GtkDropDownButtonClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) gtk_drop_down_button_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (GtkDropDownButton),
	0,		/* n_preallocs */
	(GInstanceInitFunc) gtk_drop_down_button_init,
      };

      option_menu_type =
	g_type_register_static (GTK_TYPE_BUTTON, "GtkDropDownButton",
				&option_menu_info, 0);
    }

  return option_menu_type;
}

static void
gtk_drop_down_button_class_init (GtkDropDownButtonClass *class)
{
  GObjectClass *gobject_class;
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkButtonClass *button_class;
  GtkContainerClass *container_class;

  gobject_class = (GObjectClass*) class;
  object_class = (GtkObjectClass*) class;
  widget_class = (GtkWidgetClass*) class;
  button_class = (GtkButtonClass*) class;
  container_class = (GtkContainerClass*) class;

  parent_class = g_type_class_peek_parent (class);

  gobject_class->set_property = gtk_drop_down_button_set_property;
  gobject_class->get_property = gtk_drop_down_button_get_property;
  object_class->destroy = gtk_drop_down_button_destroy;

  widget_class->size_request = gtk_drop_down_button_size_request;
  widget_class->size_allocate = gtk_drop_down_button_size_allocate;
  widget_class->expose_event = gtk_drop_down_button_expose;
  widget_class->button_press_event = gtk_drop_down_button_button_press;
  widget_class->key_press_event = gtk_drop_down_button_key_press;
  widget_class->show_all = gtk_drop_down_button_show_all;
  widget_class->hide_all = gtk_drop_down_button_hide_all;
  widget_class->mnemonic_activate = gtk_drop_down_button_mnemonic_activate;

  g_object_class_install_property (gobject_class,
                                   PROP_MENU,
                                   g_param_spec_object ("menu",
                                                        "Menu",
                                                        "The menu of options",
                                                        GTK_TYPE_MENU,
                                                        G_PARAM_READABLE | G_PARAM_WRITABLE));

  gtk_widget_class_install_style_property (widget_class,
					   g_param_spec_boxed ("indicator_size",
							       "Indicator Size",
							       "Size of dropdown indicator",
							       GTK_TYPE_REQUISITION,
							       G_PARAM_READABLE));
  gtk_widget_class_install_style_property (widget_class,
					   g_param_spec_boxed ("indicator_spacing",
							       "Indicator Spacing",
							       "Spacing around indicator",
							       GTK_TYPE_BORDER,
							       G_PARAM_READABLE));
}

static void
gtk_drop_down_button_init (GtkDropDownButton *button)
{
  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS (button, GTK_CAN_DEFAULT | GTK_RECEIVES_DEFAULT);

  button->menu = NULL;
}

GtkWidget*
gtk_drop_down_button_new (void)
{
  return g_object_new (GTK_TYPE_DROP_DOWN_BUTTON, NULL);
}

GtkWidget*
gtk_drop_down_button_get_menu (GtkDropDownButton *button)
{
  g_return_val_if_fail (GTK_IS_DROP_DOWN_BUTTON (button), NULL);

  return button->menu;
}

static void
gtk_drop_down_button_detacher (GtkWidget     *widget,
			  GtkMenu	*menu)
{
  GtkDropDownButton *button;

  g_return_if_fail (GTK_IS_DROP_DOWN_BUTTON (widget));

  button = GTK_DROP_DOWN_BUTTON (widget);
  g_return_if_fail (button->menu == (GtkWidget*) menu);

  g_signal_handlers_disconnect_by_func (button->menu, gtk_widget_queue_draw, button);

  button->menu = NULL;
  g_object_notify (G_OBJECT (button), "menu");
}

void
gtk_drop_down_button_set_menu (GtkDropDownButton *button,
			  GtkWidget     *menu)
{
  g_return_if_fail (GTK_IS_DROP_DOWN_BUTTON (button));
  g_return_if_fail (GTK_IS_MENU (menu));

  if (button->menu != menu)
    {
      if (button->menu)
      {
         if (GTK_MENU_SHELL (button->menu)->active)
            g_signal_emit_by_name (button->menu, "cancel", 0);

            gtk_menu_detach (GTK_MENU (button->menu));
      }

      button->menu = menu;

      if (menu)
      {
         gtk_menu_attach_to_widget (GTK_MENU (menu),
				 GTK_WIDGET (button),
				 gtk_drop_down_button_detacher);

	g_signal_connect_swapped(button->menu, "selection_done",
			G_CALLBACK (gtk_widget_queue_draw),
			button);

         if (GTK_WIDGET (button)->parent)
            gtk_widget_queue_resize (GTK_WIDGET (button));
      }

      g_object_notify (G_OBJECT (button), "menu");
    }
}

static void
gtk_drop_down_button_set_property (GObject            *object,
			      guint               prop_id,
			      const GValue       *value,
			      GParamSpec         *pspec)
{
  GtkDropDownButton *button = GTK_DROP_DOWN_BUTTON (object);

  switch (prop_id)
    {
    case PROP_MENU:
      gtk_drop_down_button_set_menu (button, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
gtk_drop_down_button_get_property (GObject            *object,
			      guint               prop_id,
			      GValue             *value,
			      GParamSpec         *pspec)
{
  GtkDropDownButton *button = GTK_DROP_DOWN_BUTTON (object);

  switch (prop_id)
    {
    case PROP_MENU:
      g_value_set_object (value, button->menu);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
gtk_drop_down_button_destroy (GtkObject *object)
{
  GtkDropDownButton *button;

  g_return_if_fail (GTK_IS_DROP_DOWN_BUTTON (object));

  button = GTK_DROP_DOWN_BUTTON (object);

  if (button->menu)
    gtk_widget_destroy(button->menu);

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
gtk_drop_down_button_get_props (GtkDropDownButton       *button,
			   GtkDropDownButtonProps  *props)
{
  GtkRequisition *indicator_size;
  GtkBorder *indicator_spacing;

  gtk_widget_style_get (GTK_WIDGET (button),
			"indicator_size", &indicator_size,
			"indicator_spacing", &indicator_spacing,
			"interior_focus", &props->interior_focus,
			"focus_line_width", &props->focus_width,
			"focus_padding", &props->focus_pad,
			NULL);

  if (indicator_size)
    props->indicator_size = *indicator_size;
  else
    props->indicator_size = default_props.indicator_size;

  if (indicator_spacing)
    props->indicator_spacing = *indicator_spacing;
  else
    props->indicator_spacing = default_props.indicator_spacing;

  g_free (indicator_size);
  g_free (indicator_spacing);
}

static void
gtk_drop_down_button_size_request (GtkWidget      *widget,
			      GtkRequisition *requisition)
{
  GtkDropDownButton *button = GTK_DROP_DOWN_BUTTON (widget);
  GtkDropDownButtonProps props;
  gint tmp;
  GtkRequisition child_requisition = { 0, 0 };

  gtk_drop_down_button_get_props (button, &props);

  if (GTK_BIN (button)->child && GTK_WIDGET_VISIBLE (GTK_BIN (button)->child))
    gtk_widget_size_request (GTK_BIN (button)->child, &child_requisition);

  requisition->width = ((GTK_CONTAINER (widget)->border_width +
			 GTK_WIDGET (widget)->style->xthickness + props.focus_pad) * 2 +
			child_requisition.width +
 			props.indicator_size.width +
 			props.indicator_spacing.left + props.indicator_spacing.right +
			CHILD_LEFT_SPACING + CHILD_RIGHT_SPACING + props.focus_width * 2);
  requisition->height = ((GTK_CONTAINER (widget)->border_width +
			  GTK_WIDGET (widget)->style->ythickness + props.focus_pad) * 2 +
			 child_requisition.height +
			 CHILD_TOP_SPACING + CHILD_BOTTOM_SPACING + props.focus_width * 2);

  tmp = (requisition->height - child_requisition.height +
	 props.indicator_size.height + props.indicator_spacing.top + props.indicator_spacing.bottom);
  requisition->height = MAX (requisition->height, tmp);
}

static void
gtk_drop_down_button_size_allocate (GtkWidget     *widget,
			       GtkAllocation *allocation)
{
  GtkWidget *child;
  GtkButton *button = GTK_BUTTON (widget);
  GtkAllocation child_allocation;
  GtkDropDownButtonProps props;
  gint border_width;

  gtk_drop_down_button_get_props (GTK_DROP_DOWN_BUTTON (widget), &props);
  border_width = GTK_CONTAINER (widget)->border_width;

  widget->allocation = *allocation;
  if (GTK_WIDGET_REALIZED (widget))
    gdk_window_move_resize (button->event_window,
			    allocation->x + border_width, allocation->y + border_width,
			    allocation->width - border_width * 2, allocation->height - border_width * 2);

  child = GTK_BIN (widget)->child;
  if (child && GTK_WIDGET_VISIBLE (child))
    {
      gint xthickness = GTK_WIDGET (widget)->style->xthickness;
      gint ythickness = GTK_WIDGET (widget)->style->ythickness;

      child_allocation.x = widget->allocation.x + border_width + xthickness + props.focus_width + props.focus_pad + CHILD_LEFT_SPACING;
      child_allocation.y = widget->allocation.y + border_width + ythickness + props.focus_width + props.focus_pad + CHILD_TOP_SPACING;
      child_allocation.width = MAX (1, allocation->width - (border_width + xthickness + props.focus_width + props.focus_pad) * 2 -
				    props.indicator_size.width - props.indicator_spacing.left - props.indicator_spacing.right -
				    CHILD_LEFT_SPACING - CHILD_RIGHT_SPACING);
      child_allocation.height = MAX (1, allocation->height - (border_width + ythickness + props.focus_width + props.focus_pad) * 2 -
				     CHILD_TOP_SPACING - CHILD_BOTTOM_SPACING);

      if (gtk_widget_get_direction (GTK_WIDGET (widget)) == GTK_TEXT_DIR_RTL)
	child_allocation.x += props.indicator_size.width + props.indicator_spacing.left + props.indicator_spacing.right;

      gtk_widget_size_allocate (child, &child_allocation);
    }
}

static void
gtk_drop_down_button_paint (GtkWidget    *widget, GdkRectangle *area)
{
    GdkRectangle button_area;
    GtkDropDownButtonProps props;
    gint border_width;
    gint arrow_width, arrow_height;
    gint x,y;

    g_return_if_fail (GTK_IS_DROP_DOWN_BUTTON (widget));
    g_return_if_fail (area != NULL);

    if (GTK_WIDGET_DRAWABLE (widget))
    {
	border_width = GTK_CONTAINER (widget)->border_width;
	gtk_drop_down_button_get_props (GTK_DROP_DOWN_BUTTON (widget), &props);

	if (!GTK_MENU_SHELL (GTK_DROP_DOWN_BUTTON (widget)->menu)->active)
		_gtk_dropdownbutton_paint (GTK_BUTTON(widget),
			area,
			GTK_WIDGET_STATE (widget),
			GTK_SHADOW_OUT,
			"dropdownbutton",
			"dropdownbuttondefault");
	else
	{
		gtk_widget_set_state (widget, GTK_STATE_ACTIVE);
		_gtk_dropdownbutton_paint (GTK_BUTTON(widget),
			area,
			GTK_WIDGET_STATE (widget),
			GTK_SHADOW_IN,
			"dropdownbutton",
			"dropdownbuttondefault");
		gtk_widget_set_state (widget, GTK_STATE_NORMAL);
	}

	arrow_width  = props.indicator_size.width + (props.indicator_size.width % 2) - 1;
	arrow_height = arrow_width / 2 + 1;

	button_area.x = widget->allocation.x + border_width;
	button_area.y = widget->allocation.y + border_width;
	button_area.width = widget->allocation.width - 2 * border_width;
	button_area.height = widget->allocation.height - 2 * border_width;

	if (!props.interior_focus && GTK_WIDGET_HAS_FOCUS (widget))
	{
	  button_area.x += props.focus_width + props.focus_pad;
	  button_area.y += props.focus_width + props.focus_pad;
	  button_area.width -= 2 * (props.focus_width + props.focus_pad);
	  button_area.height -= 2 * (props.focus_width + props.focus_pad);
	}

      if (gtk_widget_get_direction (GTK_WIDGET (widget)) == GTK_TEXT_DIR_RTL)
	x = button_area.x + props.indicator_spacing.right +
	  widget->style->xthickness;
      else
	x = button_area.x + button_area.width -
	  props.indicator_size.width - props.indicator_spacing.right -
	  widget->style->xthickness;
      y = button_area.y + (button_area.height - arrow_height) / 2;

      gtk_draw_arrow (widget->style, widget->window, GTK_WIDGET_STATE (widget),
		GTK_SHADOW_NONE, GTK_ARROW_DOWN, TRUE,
		x,y,arrow_width,arrow_height);

      if (GTK_WIDGET_HAS_FOCUS (widget))
	{
	  if (props.interior_focus)
	    {
	      button_area.x += widget->style->xthickness + props.focus_pad;
	      button_area.y += widget->style->ythickness + props.focus_pad;
	      button_area.width -= 2 * (widget->style->xthickness + props.focus_pad) +
		      props.indicator_spacing.left +
		      props.indicator_spacing.right +
		      props.indicator_size.width;
	      button_area.height -= 2 * (widget->style->ythickness + props.focus_pad);
	      if (gtk_widget_get_direction (GTK_WIDGET (widget)) == GTK_TEXT_DIR_RTL)
		button_area.x += props.indicator_spacing.left +
		  props.indicator_spacing.right +
		  props.indicator_size.width;
	    }
	  else
	    {
	      button_area.x -= props.focus_width + props.focus_pad;
	      button_area.y -= props.focus_width + props.focus_pad;
	      button_area.width += 2 * (props.focus_width + props.focus_pad);
	      button_area.height += 2 * (props.focus_width + props.focus_pad);
	    }

	    gtk_paint_focus (widget->style, widget->window, GTK_WIDGET_STATE (widget),
			   area, widget, "dropdownbutton",
			   button_area.x,
			   button_area.y,
			   button_area.width,
			   button_area.height);
	}
    }
}

static gint
gtk_drop_down_button_expose (GtkWidget      *widget,
			GdkEventExpose *event)
{
  g_return_val_if_fail (GTK_IS_DROP_DOWN_BUTTON (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  if (GTK_WIDGET_DRAWABLE (widget))
    {
      gtk_drop_down_button_paint (widget, &event->area);

      if (GTK_BIN (widget)->child)
	gtk_container_propagate_expose (GTK_CONTAINER (widget),
					GTK_BIN (widget)->child,
					event);
    }

  return FALSE;
}

static gint
gtk_drop_down_button_button_press (GtkWidget      *widget,
			      GdkEventButton *event)
{
  GtkDropDownButton *button;

  g_return_val_if_fail (GTK_IS_DROP_DOWN_BUTTON (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  button = GTK_DROP_DOWN_BUTTON (widget);

  if ((event->type == GDK_BUTTON_PRESS) &&
      (event->button == 1))
    {
      gtk_menu_popup (GTK_MENU (button->menu), NULL, NULL,
		      gtk_drop_down_button_position, button,
		      event->button, event->time);
      return TRUE;
    }

  return FALSE;
}

static gint
gtk_drop_down_button_key_press (GtkWidget   *widget,
			   GdkEventKey *event)
{
  GtkDropDownButton *button;

  g_return_val_if_fail (GTK_IS_DROP_DOWN_BUTTON (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  button = GTK_DROP_DOWN_BUTTON (widget);

  switch (event->keyval)
    {
    case GDK_KP_Space:
    case GDK_space:
      gtk_menu_popup (GTK_MENU (button->menu), NULL, NULL,
		      gtk_drop_down_button_position, button,
		      0, event->time);
      return TRUE;
    }

  return FALSE;
}

static void
gtk_drop_down_button_position (GtkMenu  *menu,
			  gint     *x,
			  gint     *y,
			  gboolean *push_in,
			  gpointer  user_data)
{
  GtkDropDownButton *button;
  GtkWidget *active;
  GtkWidget *widget;
  GtkRequisition requisition;
  gint screen_width;
  gint menu_xpos;
  gint menu_ypos;
  gint menu_width;

  g_return_if_fail (GTK_IS_DROP_DOWN_BUTTON (user_data));

  button = GTK_DROP_DOWN_BUTTON (user_data);
  widget = GTK_WIDGET (button);

  gtk_widget_get_child_requisition (GTK_WIDGET (menu), &requisition);
  menu_width = requisition.width;

  active = gtk_menu_get_active (GTK_MENU (button->menu));
  gdk_window_get_origin (widget->window, &menu_xpos, &menu_ypos);

  menu_xpos += widget->allocation.x;
  menu_ypos += widget->allocation.y + widget->allocation.height - 2;

  screen_width = gdk_screen_get_width (gtk_widget_get_screen (widget));
  
  if (menu_xpos < 0)
    menu_xpos = 0;
  else if ((menu_xpos + menu_width) > screen_width)
    menu_xpos -= ((menu_xpos + menu_width) - screen_width);

  *x = menu_xpos;
  *y = menu_ypos;
  *push_in = TRUE;
}


static void
gtk_drop_down_button_show_all (GtkWidget *widget)
{
  GtkContainer *container;
  GtkDropDownButton *button;
  
  g_return_if_fail (GTK_IS_DROP_DOWN_BUTTON (widget));
  container = GTK_CONTAINER (widget);
  button = GTK_DROP_DOWN_BUTTON (widget);

  gtk_widget_show (widget);
  gtk_container_foreach (container, (GtkCallback) gtk_widget_show_all, NULL);
  if (button->menu)
    gtk_widget_show_all (button->menu);
}


static void
gtk_drop_down_button_hide_all (GtkWidget *widget)
{
  GtkContainer *container;

  g_return_if_fail (GTK_IS_DROP_DOWN_BUTTON (widget));
  container = GTK_CONTAINER (widget);

  gtk_widget_hide (widget);
  gtk_container_foreach (container, (GtkCallback) gtk_widget_hide_all, NULL);
}

static gboolean
gtk_drop_down_button_mnemonic_activate (GtkWidget *widget,
				   gboolean   group_cycling)
{
  gtk_widget_grab_focus (widget);
  return TRUE;
}

