#ifndef __PORT_LAYOUT_H
#define __PORT_LAYOUT_H

#include "Types.h"

#define PORT_TYPE_LAYOUT            (port_layout_get_type ())
#define PORT_LAYOUT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), PORT_TYPE_LAYOUT, PortLayout))
#define PORT_LAYOUT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), PORT_TYPE_LAYOUT, PortLayoutClass))
#define PORT_IS_LAYOUT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), PORT_TYPE_LAYOUT))
#define PORT_IS_LAYOUT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), PORT_TYPE_LAYOUT))
#define PORT_LAYOUT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), PORT_TYPE_LAYOUT, PortLayoutClass))


typedef struct _PortLayout        PortLayout;
typedef struct _PortLayoutClass   PortLayoutClass;

struct _PortLayout
{
  GtkContainer container;

  GList *children;

  guint width;
  guint height;

  GtkAdjustment *hadjustment;
  GtkAdjustment *vadjustment;

  /*< public >*/
  GdkWindow *bin_window;

  /*< private >*/
  GdkVisibilityState visibility;
  gint scroll_x;
  gint scroll_y;
  GtkRequisition requisition;
};

struct _PortLayoutClass
{
  GtkContainerClass parent_class;

  void  (*set_scroll_adjustments)   (PortLayout	    *layout,
				     GtkAdjustment  *hadjustment,
				     GtkAdjustment  *vadjustment);
};

GType          port_layout_get_type        (void) G_GNUC_CONST;
GtkWidget*   port_layout_new  (GtkAdjustment *hadjustment, GtkAdjustment *vadjustment);
void           port_layout_put  (PortLayout *layout, GtkWidget     *child_widget);
void           port_layout_set_domain (PortLayout *layout, guint width, guint height);
void           port_layout_get_domain (PortLayout *layout, guint *width, guint *height);

GtkAdjustment* port_layout_get_hadjustment (PortLayout     *layout);
GtkAdjustment* port_layout_get_vadjustment (PortLayout     *layout);
void           port_layout_set_hadjustment (PortLayout *layout, GtkAdjustment *adjustment);
void           port_layout_set_vadjustment (PortLayout *layout, GtkAdjustment *adjustment);

#endif /* __PORT_LAYOUT_H */
