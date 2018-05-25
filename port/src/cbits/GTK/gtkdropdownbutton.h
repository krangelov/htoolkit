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

#ifndef __GTK_DROP_DOWN_BUTTON_H__
#define __GTK_DROP_DOWN_BUTTON_H__


#include <gdk/gdk.h>
#include <gtk/gtkbutton.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_TYPE_DROP_DOWN_BUTTON              (gtk_drop_down_button_get_type ())
#define GTK_DROP_DOWN_BUTTON(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_DROP_DOWN_BUTTON, GtkDropDownButton))
#define GTK_DROP_DOWN_BUTTON_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_DROP_DOWN_BUTTON, GtkDropDownButtonClass))
#define GTK_IS_DROP_DOWN_BUTTON(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_DROP_DOWN_BUTTON))
#define GTK_IS_DROP_DOWN_BUTTON_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_DROP_DOWN_BUTTON))
#define GTK_DROP_DOWN_BUTTON_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_DROP_DOWN_BUTTON, GtkDropDownButtonClass))


typedef struct _GtkDropDownButton       GtkDropDownButton;
typedef struct _GtkDropDownButtonClass  GtkDropDownButtonClass;

struct _GtkDropDownButton
{
  GtkButton button;
  
  GtkWidget *menu;
};

struct _GtkDropDownButtonClass
{
  GtkButtonClass parent_class;
};

GType      gtk_drop_down_button_get_type    (void) G_GNUC_CONST;
GtkWidget* gtk_drop_down_button_new         (void);
GtkWidget* gtk_drop_down_button_get_menu    (GtkDropDownButton *option_menu);
void       gtk_drop_down_button_set_menu    (GtkDropDownButton *option_menu, GtkWidget *menu);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_DROP_DOWN_BUTTON_H__ */
