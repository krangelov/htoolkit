/* gtkdropdowntoolbutton.c
 *
 * Copyright (C) 2002 Anders Carlsson <andersca@codefactory.se>
 * Copyright (C) 2002 James Henstridge <james@daa.com.au>
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

 #include <gtk/gtk.h>
#include "gtkdropdowntoolbutton.h"
#include "gtkdropdownbutton.h"

#define MENU_ID "gtk-drop-down-tool-button-menu-id"

static void gtk_drop_down_tool_button_init       (GtkDropDownToolButton      *button);
static void gtk_drop_down_tool_button_class_init (GtkDropDownToolButtonClass *klass);

static gboolean gtk_drop_down_tool_button_create_menu_proxy (GtkToolItem *button);

static GObjectClass *parent_class = NULL;

GType
gtk_drop_down_tool_button_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
	{
	  sizeof (GtkDropDownToolButtonClass),
	  (GBaseInitFunc) 0,
	  (GBaseFinalizeFunc) 0,
	  (GClassInitFunc) gtk_drop_down_tool_button_class_init,
	  (GClassFinalizeFunc) 0,
	  NULL,
	  sizeof (GtkDropDownToolButton),
	  0, /* n_preallocs */
	  (GInstanceInitFunc) gtk_drop_down_tool_button_init
	};

      type = g_type_register_static (GTK_TYPE_TOOL_BUTTON,
				     "GtkdropdownToolButton", &type_info, 0);
    }
  return type;
}

GtkWidget *_gtk_tool_button_get_button (GtkToolButton *button)
{
  return GTK_BIN(button)->child;
}

static void
gtk_drop_down_tool_button_class_init (GtkDropDownToolButtonClass *klass)
{
  GObjectClass *object_class;
  GtkToolItemClass *toolitem_class;
  GtkToolButtonClass *toolbutton_class;

  parent_class = g_type_class_peek_parent (klass);

  object_class = (GObjectClass *)klass;
  toolitem_class = (GtkToolItemClass *)klass;
  toolbutton_class = (GtkToolButtonClass *)klass;

  toolitem_class->create_menu_proxy = gtk_drop_down_tool_button_create_menu_proxy;
  toolbutton_class->button_type = GTK_TYPE_DROP_DOWN_BUTTON;
}

static void
gtk_drop_down_tool_button_init (GtkDropDownToolButton *button)
{
}

static GtkWidget *
clone_image_menu_size (GtkImage *image, GtkSettings *settings)
{
  GtkImageType storage_type = gtk_image_get_storage_type (image);

  if (storage_type == GTK_IMAGE_STOCK)
    {
      gchar *stock_id;
      gtk_image_get_stock (image, &stock_id, NULL);
      return gtk_image_new_from_stock (stock_id, GTK_ICON_SIZE_MENU);
    }
  else if (storage_type == GTK_IMAGE_ICON_SET)
    {
      GtkIconSet *icon_set;
      gtk_image_get_icon_set (image, &icon_set, NULL);
      return gtk_image_new_from_icon_set (icon_set, GTK_ICON_SIZE_MENU);
    }
  else if (storage_type == GTK_IMAGE_PIXBUF)
    {
      gint width, height;

      if (settings &&
	  gtk_icon_size_lookup_for_settings (settings, GTK_ICON_SIZE_MENU,
					     &width, &height))
	{
	  GdkPixbuf *src_pixbuf, *dest_pixbuf;

	  src_pixbuf = gtk_image_get_pixbuf (image);
	  dest_pixbuf = gdk_pixbuf_scale_simple (src_pixbuf, width, height,
						 GDK_INTERP_BILINEAR);

	  return gtk_image_new_from_pixbuf (dest_pixbuf);
	}
    }

  return NULL;
}

static gboolean
gtk_drop_down_tool_button_create_menu_proxy (GtkToolItem *item)
{
  GtkToolButton *tool_button = GTK_TOOL_BUTTON (item);
  GtkWidget *menu_item = NULL;
  GtkStockItem stock_item;
  gboolean use_mnemonic = TRUE;
  const char *label;
  GtkWidget *menu_image = NULL;

  GtkWidget *label_widget = gtk_tool_button_get_label_widget (tool_button);
  GtkWidget *icon_widget = gtk_tool_button_get_icon_widget(tool_button);
  GtkWidget *menu_widget = GTK_DROP_DOWN_BUTTON(_gtk_tool_button_get_button (GTK_TOOL_BUTTON(tool_button)))->menu;
  const gchar *label_text = gtk_tool_button_get_label (tool_button);
  const gchar *stock_id = gtk_tool_button_get_stock_id (tool_button);

   if (label_widget && GTK_IS_LABEL (label_widget))
    {
      label = gtk_label_get_label (GTK_LABEL (label_widget));
      use_mnemonic = gtk_label_get_use_underline (GTK_LABEL (label_widget));
    }
  else if (label_text)
    {
      label = label_text;
      use_mnemonic = gtk_tool_button_get_use_underline (tool_button);
    }
  else if (stock_id && gtk_stock_lookup (stock_id, &stock_item))
    {
      label = stock_item.label;
    }
  else
    {
      label = "";
    }

  if (use_mnemonic)
    menu_item = gtk_image_menu_item_new_with_mnemonic (label);
  else
    menu_item = gtk_image_menu_item_new_with_label (label);

  if (icon_widget && GTK_IS_IMAGE (icon_widget))
    {
      menu_image = clone_image_menu_size (GTK_IMAGE(icon_widget),
					  gtk_widget_get_settings (GTK_WIDGET (tool_button)));
    }
  else if (stock_id)
    {
      menu_image = gtk_image_new_from_stock (stock_id, GTK_ICON_SIZE_MENU);
    }

  if (menu_image)
    gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu_item), menu_image);

  gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu_item), menu_widget);

  gtk_tool_item_set_proxy_menu_item (item, MENU_ID, menu_item);

  return TRUE;
}

GtkToolItem *
gtk_drop_down_tool_button_new (GtkWidget   *icon_widget,
					     const gchar *label,
					     GtkWidget *menu)
{
  GtkToolButton *button;

  button = g_object_new (GTK_TYPE_DROP_DOWN_TOOL_BUTTON,
			 NULL);

  if (label)
    gtk_tool_button_set_label (button, label);

  if (icon_widget)
    gtk_tool_button_set_icon_widget (button, icon_widget);

  if (menu)
    gtk_drop_down_tool_button_set_menu(GTK_DROP_DOWN_TOOL_BUTTON(button), menu);

  return GTK_TOOL_ITEM (button);
}

GtkToolItem *
gtk_drop_down_tool_button_new_from_stock (const gchar *stock_id)
{
  GtkToolButton *button;

  g_return_val_if_fail (stock_id != NULL, NULL);
  
  button = g_object_new (GTK_TYPE_DROP_DOWN_TOOL_BUTTON,
			 "stock_id", stock_id,
			 NULL);

  return GTK_TOOL_ITEM (button);
}

GtkWidget* gtk_drop_down_tool_button_get_menu(GtkDropDownToolButton *button)
{
	return gtk_drop_down_button_get_menu(
			GTK_DROP_DOWN_BUTTON(_gtk_tool_button_get_button (GTK_TOOL_BUTTON(button))));
}

void gtk_drop_down_tool_button_set_menu(GtkDropDownToolButton *button, GtkWidget *menu)
{
	gtk_drop_down_button_set_menu(
		GTK_DROP_DOWN_BUTTON(_gtk_tool_button_get_button (GTK_TOOL_BUTTON(button))),
		menu);
}
