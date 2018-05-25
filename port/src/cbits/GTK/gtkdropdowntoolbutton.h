/* gtkdropdowntoolbutton.h
 *
 * Copyright (C) 2002 Anders Carlsson <andersca@codefactory.se>
 * Copyright (C) 2002 James Henstridge <james@daa.com.au>
 * Copyright (C) 2003 Soeren Sandmann <sandmann@daimi.au.dk>
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

#ifndef __GTK_DROP_DOWN_TOOL_BUTTON_H__
#define __GTK_DROP_DOWN_TOOL_BUTTON_H__

G_BEGIN_DECLS

#define GTK_TYPE_DROP_DOWN_TOOL_BUTTON            (gtk_drop_down_tool_button_get_type ())
#define GTK_DROP_DOWN_TOOL_BUTTON(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_DROP_DOWN_TOOL_BUTTON, GtkDropDownToolButton))
#define GTK_DROP_DOWN_TOOL_BUTTON_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_DROP_DOWN_TOOL_BUTTON, GtkDropDownToolButtonClass))
#define GTK_IS_DROP_DOWN_TOOL_BUTTON(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_DROP_DOWN_TOOL_BUTTON))
#define GTK_IS_DROP_DOWN_TOOL_BUTTON_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), GTK_TYPE_DROP_DOWN_TOOL_BUTTON))
#define GTK_DROP_DOWN_TOOL_BUTTON_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), GTK_TYPE_DROP_DOWN_TOOL_BUTTON, GtkDropDownToolButtonClass))

typedef struct _GtkDropDownToolButton      GtkDropDownToolButton;
typedef struct _GtkDropDownToolButtonClass GtkDropDownToolButtonClass;

struct _GtkDropDownToolButton
{
  GtkToolButton parent;

  /*< private >*/
  GtkWidget *menu_item;
};

struct _GtkDropDownToolButtonClass
{
  GtkToolButtonClass parent_class;
};

GType        gtk_drop_down_tool_button_get_type       (void) G_GNUC_CONST;
GtkToolItem *gtk_drop_down_tool_button_new            (GtkWidget   *icon_widget, const gchar *label, GtkWidget *menu);
GtkToolItem *gtk_drop_down_tool_button_new_from_stock (const gchar *stock_id);
GtkWidget* gtk_drop_down_tool_button_get_menu    (GtkDropDownToolButton *button);
void       gtk_drop_down_tool_button_set_menu    (GtkDropDownToolButton *button, GtkWidget *menu);

G_END_DECLS

#endif /* __GTK_dropdown_TOOL_BUTTON_H__ */
