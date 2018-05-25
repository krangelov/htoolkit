#include "LayoutContainer.h"
#include "Handlers_stub.h"

enum {
   PROP_0,
   PROP_HADJUSTMENT,
   PROP_VADJUSTMENT,
   PROP_WIDTH,
   PROP_HEIGHT
};

static void port_layout_class_init (PortLayoutClass *class);
static void port_layout_get_property       (GObject        *object,
                                           guint           prop_id,
                                           GValue         *value,
                                           GParamSpec     *pspec);
static void port_layout_set_property       (GObject        *object,
                                           guint           prop_id,
                                           const GValue   *value,
                                           GParamSpec     *pspec);
static GObject *port_layout_constructor    (GType                  type,
					   guint                  n_properties,
					   GObjectConstructParam *properties);
static void port_layout_init               (PortLayout      *layout);
static void port_layout_finalize           (GObject        *object);
static void port_layout_realize            (GtkWidget      *widget);
static void port_layout_unrealize          (GtkWidget      *widget);
static void port_layout_map                (GtkWidget      *widget);
static void port_layout_size_request (GtkWidget *widget, GtkRequisition *requisition);
static void port_layout_size_allocate (GtkWidget *widget, GtkAllocation  *allocation);
static gint port_layout_expose             (GtkWidget      *widget,
                                           GdkEventExpose *event);
static void port_layout_remove             (GtkContainer   *container,
                                           GtkWidget      *widget);
static void port_layout_forall             (GtkContainer   *container,
                                           gboolean        include_internals,
                                           GtkCallback     callback,
                                           gpointer        callback_data);
static void port_layout_set_adjustments    (PortLayout *layout, GtkAdjustment  *hadj, GtkAdjustment *vadj);
static void port_layout_adjustment_changed (GtkAdjustment  *adjustment, PortLayout *layout);
static void port_layout_style_set(GtkWidget      *widget, GtkStyle       *old_style);
static void port_layout_set_adjustment_upper (GtkAdjustment *adj, gdouble upper, gboolean always_emit_changed);

static GtkWidgetClass *parent_class = NULL;

GtkWidget* port_layout_new (GtkAdjustment *hadjustment, GtkAdjustment *vadjustment)
{
  GtkLayout *layout;

  layout = g_object_new (PORT_TYPE_LAYOUT,
			 "hadjustment", hadjustment,
			 "vadjustment", vadjustment,
			 NULL);

  return GTK_WIDGET (layout);
}

GtkAdjustment* port_layout_get_hadjustment (PortLayout     *layout)
{
  g_return_val_if_fail (PORT_IS_LAYOUT (layout), NULL);

  return layout->hadjustment;
}

GtkAdjustment* port_layout_get_vadjustment (PortLayout *layout)
{
  g_return_val_if_fail (PORT_IS_LAYOUT (layout), NULL);

  return layout->vadjustment;
}

static GtkAdjustment *new_default_adjustment (void)
{
  return GTK_ADJUSTMENT(gtk_adjustment_new (0.0, 0.0, 0.0, 0.0, 0.0, 0.0));
}

static void port_layout_set_adjustments (PortLayout *layout, GtkAdjustment *hadj, GtkAdjustment *vadj)
{
  gboolean need_adjust = FALSE;

  g_return_if_fail (PORT_IS_LAYOUT (layout));

  if (hadj)
    g_return_if_fail (GTK_IS_ADJUSTMENT (hadj));
  else if (layout->hadjustment)
    hadj = new_default_adjustment ();
  if (vadj)
    g_return_if_fail (GTK_IS_ADJUSTMENT (vadj));
  else if (layout->vadjustment)
    vadj = new_default_adjustment ();

  if (layout->hadjustment && (layout->hadjustment != hadj))
    {
      g_signal_handlers_disconnect_by_func (layout->hadjustment,
					    port_layout_adjustment_changed,
					    layout);
      g_object_unref (layout->hadjustment);
    }

  if (layout->vadjustment && (layout->vadjustment != vadj))
    {
      g_signal_handlers_disconnect_by_func (layout->vadjustment,
					    port_layout_adjustment_changed,
					    layout);
      g_object_unref (layout->vadjustment);
    }
  
  if (layout->hadjustment != hadj)
    {
      layout->hadjustment = hadj;
      g_object_ref (layout->hadjustment);
      gtk_object_sink (GTK_OBJECT (layout->hadjustment));
      port_layout_set_adjustment_upper (layout->hadjustment, layout->width, FALSE);
      
      g_signal_connect (layout->hadjustment, "value_changed",
			G_CALLBACK (port_layout_adjustment_changed),
			layout);
      need_adjust = TRUE;
    }
  
  if (layout->vadjustment != vadj)
    {
      layout->vadjustment = vadj;
      g_object_ref (layout->vadjustment);
      gtk_object_sink (GTK_OBJECT (layout->vadjustment));
      port_layout_set_adjustment_upper (layout->vadjustment, layout->height, FALSE);

      g_signal_connect (layout->vadjustment, "value_changed",
			G_CALLBACK (port_layout_adjustment_changed),
			layout);
      need_adjust = TRUE;
    }

  /* vadj or hadj can be NULL while constructing; don't emit a signal
     then */
  if (need_adjust && vadj && hadj)
    port_layout_adjustment_changed (NULL, layout);
}

static void port_layout_finalize (GObject *object)
{
  PortLayout *layout = PORT_LAYOUT (object);

  g_object_unref (layout->hadjustment);
  g_object_unref (layout->vadjustment);

  G_OBJECT_CLASS (parent_class)->finalize (object);
}

void port_layout_set_hadjustment(PortLayout *layout, GtkAdjustment *adjustment)
{
  g_return_if_fail (PORT_IS_LAYOUT (layout));

  port_layout_set_adjustments (layout, adjustment, layout->vadjustment);
  g_object_notify (G_OBJECT (layout), "hadjustment");
}

void port_layout_set_vadjustment (PortLayout *layout, GtkAdjustment *adjustment)
{
  g_return_if_fail (PORT_IS_LAYOUT (layout));

  port_layout_set_adjustments (layout, layout->hadjustment, adjustment);
  g_object_notify (G_OBJECT (layout), "vadjustment");
}

void port_layout_put (PortLayout *layout, GtkWidget *child_widget)
{
  g_return_if_fail (PORT_IS_LAYOUT (layout));
  g_return_if_fail (GTK_IS_WIDGET (child_widget));

  layout->children = g_list_append (layout->children, child_widget);

  if (GTK_WIDGET_REALIZED (layout))
    gtk_widget_set_parent_window (child_widget, layout->bin_window);

  gtk_widget_set_parent (child_widget, GTK_WIDGET (layout));
}

static void port_layout_set_adjustment_upper (GtkAdjustment *adj,
				 gdouble        upper,
				 gboolean       always_emit_changed)
{
  gboolean changed = FALSE;
  gboolean value_changed = FALSE;

  gdouble min = MAX (0., upper - adj->page_size);

  if (upper != adj->upper)
    {
      adj->upper = upper;
      changed = TRUE;
    }

  if (adj->value > min)
    {
      adj->value = min;
      value_changed = TRUE;
    }

  if (changed || always_emit_changed)
    gtk_adjustment_changed (adj);
  if (value_changed)
    gtk_adjustment_value_changed (adj);
}

void port_layout_set_domain(PortLayout *layout, guint width, guint height)
{
  GtkWidget *widget;

  g_return_if_fail (PORT_IS_LAYOUT (layout));

  widget = GTK_WIDGET (layout);

  g_object_freeze_notify (G_OBJECT (layout));
  if (width != layout->width)
     {
	layout->width = width;
	g_object_notify (G_OBJECT (layout), "width");
     }
  if (height != layout->height)
     {
	layout->height = height;
	g_object_notify (G_OBJECT (layout), "height");
     }
  g_object_thaw_notify (G_OBJECT (layout));

  if (layout->hadjustment)
    port_layout_set_adjustment_upper (layout->hadjustment, layout->width, FALSE);
  if (layout->vadjustment)
    port_layout_set_adjustment_upper (layout->vadjustment, layout->height, FALSE);

  if (GTK_WIDGET_REALIZED (layout))
    {
      width = MAX (width, widget->allocation.width);
      height = MAX (height, widget->allocation.height);
      gdk_window_resize (layout->bin_window, width, height);
    }
}

void port_layout_get_domain(PortLayout *layout, guint *width, guint     *height)
{
  g_return_if_fail (PORT_IS_LAYOUT (layout));

  if (width)
    *width = layout->width;
  if (height)
    *height = layout->height;
}

/* Basic Object handling procedures
 */
GType port_layout_get_type (void)
{
  static GType layout_type = 0;

  if (!layout_type)
    {
      static const GTypeInfo layout_info =
      {
	sizeof (PortLayoutClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) port_layout_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (PortLayout),
	0,		/* n_preallocs */
	(GInstanceInitFunc) port_layout_init,
      };

      layout_type = g_type_register_static (GTK_TYPE_CONTAINER, "PortLayout",
					    &layout_info, 0);
    }

  return layout_type;
}

void _gtk_marshal_VOID__OBJECT_OBJECT (GClosure     *closure,
                                  GValue       *return_value,
                                  guint         n_param_values,
                                  const GValue *param_values,
                                  gpointer      invocation_hint,
                                  gpointer      marshal_data)
{
  typedef void (*GMarshalFunc_VOID__OBJECT_OBJECT) (gpointer     data1,
                                                    gpointer     arg_1,
                                                    gpointer     arg_2,
                                                    gpointer     data2);
  register GMarshalFunc_VOID__OBJECT_OBJECT callback;
  register GCClosure *cc = (GCClosure*) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure))
    {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
    }
  else
    {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__OBJECT_OBJECT) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_value_get_object (param_values + 1),
            g_value_get_object (param_values + 2),
            data2);
}

static void port_layout_class_init (PortLayoutClass *class)
{
  GObjectClass *gobject_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  gobject_class = (GObjectClass*) class;
  widget_class = (GtkWidgetClass*) class;
  container_class = (GtkContainerClass*) class;

  parent_class = g_type_class_peek_parent (class);

  gobject_class->set_property = port_layout_set_property;
  gobject_class->get_property = port_layout_get_property;
  gobject_class->finalize = port_layout_finalize;
  gobject_class->constructor = port_layout_constructor;

  g_object_class_install_property (gobject_class,
				   PROP_HADJUSTMENT,
				   g_param_spec_object ("hadjustment",
							"Horizontal adjustment",
							"The GtkAdjustment for the horizontal position",
							GTK_TYPE_ADJUSTMENT,
							G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
				   PROP_VADJUSTMENT,
				   g_param_spec_object ("vadjustment",
							"Vertical adjustment",
							"The GtkAdjustment for the vertical position",
							GTK_TYPE_ADJUSTMENT,
							G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
				   PROP_WIDTH,
				   g_param_spec_uint ("width",
						     "Width",
						     "The width of the layout",
						     0,
						     G_MAXINT,
						     100,
						     G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
				   PROP_HEIGHT,
				   g_param_spec_uint ("height",
						     "Height",
						     "The height of the layout",
						     0,
						     G_MAXINT,
						     100,
						     G_PARAM_READWRITE));
  widget_class->realize = port_layout_realize;
  widget_class->unrealize = port_layout_unrealize;
  widget_class->map = port_layout_map;
  widget_class->size_request = port_layout_size_request;
  widget_class->size_allocate = port_layout_size_allocate;
  widget_class->expose_event = port_layout_expose;
  widget_class->style_set = port_layout_style_set;

  container_class->remove = port_layout_remove;
  container_class->forall = port_layout_forall;

  class->set_scroll_adjustments = port_layout_set_adjustments;

  widget_class->set_scroll_adjustments_signal =
    g_signal_new ("set_scroll_adjustments",
		  G_OBJECT_CLASS_TYPE (gobject_class),
		  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET (PortLayoutClass, set_scroll_adjustments),
		  NULL, NULL,
		  _gtk_marshal_VOID__OBJECT_OBJECT,
		  G_TYPE_NONE, 2,
		  GTK_TYPE_ADJUSTMENT,
		  GTK_TYPE_ADJUSTMENT);
}

static void port_layout_get_property (GObject     *object,
			 guint        prop_id,
			 GValue      *value,
			 GParamSpec  *pspec)
{
  PortLayout *layout = PORT_LAYOUT (object);

  switch (prop_id)
    {
    case PROP_HADJUSTMENT:
      g_value_set_object (value, layout->hadjustment);
      break;
    case PROP_VADJUSTMENT:
      g_value_set_object (value, layout->vadjustment);
      break;
    case PROP_WIDTH:
      g_value_set_uint (value, layout->width);
      break;
    case PROP_HEIGHT:
      g_value_set_uint (value, layout->height);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void port_layout_set_property (GObject      *object,
			 guint         prop_id,
			 const GValue *value,
			 GParamSpec   *pspec)
{
  PortLayout *layout = PORT_LAYOUT (object);

  switch (prop_id)
    {
    case PROP_HADJUSTMENT:
      port_layout_set_hadjustment (layout, (GtkAdjustment*) g_value_get_object (value));
      break;
    case PROP_VADJUSTMENT:
      port_layout_set_vadjustment (layout, (GtkAdjustment*) g_value_get_object (value));
      break;
    case PROP_WIDTH:
      port_layout_set_domain (layout, g_value_get_uint (value), layout->height);
      break;
    case PROP_HEIGHT:
      port_layout_set_domain (layout, layout->width, g_value_get_uint (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void port_layout_init (PortLayout *layout)
{
  layout->children = NULL;

  layout->width = 100;
  layout->height = 100;

  layout->hadjustment = NULL;
  layout->vadjustment = NULL;

  layout->bin_window = NULL;

  layout->scroll_x = 0;
  layout->scroll_y = 0;
  layout->visibility = GDK_VISIBILITY_PARTIAL;
  layout->requisition.width = 0;
  layout->requisition.height = 0;
}

static GObject *port_layout_constructor (GType                  type,
			guint                  n_properties,
			GObjectConstructParam *properties)
{
  PortLayout *layout;
  GObject *object;
  GtkAdjustment *hadj, *vadj;

  object = G_OBJECT_CLASS (parent_class)->constructor (type,
						       n_properties,
						       properties);

  layout = PORT_LAYOUT (object);

  hadj = layout->hadjustment ? layout->hadjustment : new_default_adjustment ();
  vadj = layout->vadjustment ? layout->vadjustment : new_default_adjustment ();

  if (!layout->hadjustment || !layout->vadjustment)
    port_layout_set_adjustments (layout, hadj, vadj);

  return object;
}

static void port_layout_realize (GtkWidget *widget)
{
  GList *tmp_list;
  PortLayout *layout;
  GdkWindowAttr attributes;
  gint attributes_mask;

  g_return_if_fail (PORT_IS_LAYOUT (widget));

  layout = PORT_LAYOUT (widget);
  GTK_WIDGET_SET_FLAGS (layout, GTK_REALIZED);

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);
  attributes.event_mask = GDK_VISIBILITY_NOTIFY_MASK;

  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

  widget->window = gdk_window_new (gtk_widget_get_parent_window (widget), &attributes, attributes_mask);
  gdk_window_set_user_data (widget->window, widget);

  attributes.x = - layout->hadjustment->value,
  attributes.y = - layout->vadjustment->value;
  attributes.width = MAX (layout->width, widget->allocation.width);
  attributes.height = MAX (layout->height, widget->allocation.height);
  attributes.event_mask =
	GDK_EXPOSURE_MASK |
	GDK_SCROLL_MASK |
	GDK_BUTTON_RELEASE_MASK |
	GDK_POINTER_MOTION_MASK |
	GDK_ENTER_NOTIFY_MASK |
	GDK_LEAVE_NOTIFY_MASK |
	gtk_widget_get_events (widget);

  layout->bin_window = gdk_window_new (widget->window, &attributes, attributes_mask);
  gdk_window_set_user_data (layout->bin_window, widget);

  widget->style = gtk_style_attach (widget->style, widget->window);
  gtk_style_set_background (widget->style, widget->window, GTK_STATE_NORMAL);
  gtk_style_set_background (widget->style, layout->bin_window, GTK_STATE_NORMAL);

  tmp_list = layout->children;
  while (tmp_list)
    {
      GtkWidget *child_widget = tmp_list->data;
      tmp_list = tmp_list->next;

      gtk_widget_set_parent_window (child_widget, layout->bin_window);
    }
}

static void port_layout_style_set (GtkWidget *widget, GtkStyle *old_style)
{
  if (GTK_WIDGET_CLASS (parent_class)->style_set)
    (* GTK_WIDGET_CLASS (parent_class)->style_set) (widget, old_style);

  if (GTK_WIDGET_REALIZED (widget))
    {
      gtk_style_set_background (widget->style, PORT_LAYOUT (widget)->bin_window, GTK_STATE_NORMAL);
    }
}

static void port_layout_map (GtkWidget *widget)
{
  GList *tmp_list;
  PortLayout *layout;

  g_return_if_fail (PORT_IS_LAYOUT (widget));

  layout = PORT_LAYOUT (widget);

  GTK_WIDGET_SET_FLAGS (widget, GTK_MAPPED);

  tmp_list = layout->children;
  while (tmp_list)
    {
      GtkWidget *child_widget = tmp_list->data;
      tmp_list = tmp_list->next;

      if (GTK_WIDGET_VISIBLE (child_widget))
	{
	  if (!GTK_WIDGET_MAPPED (child_widget))
	    gtk_widget_map (child_widget);
	}
    }

  gdk_window_show (layout->bin_window);
  gdk_window_show (widget->window);
}

static void port_layout_unrealize (GtkWidget *widget)
{
  PortLayout *layout;

  g_return_if_fail (PORT_IS_LAYOUT (widget));

  layout = PORT_LAYOUT (widget);

  gdk_window_set_user_data (layout->bin_window, NULL);
  gdk_window_destroy (layout->bin_window);
  layout->bin_window = NULL;

  if (GTK_WIDGET_CLASS (parent_class)->unrealize)
    (* GTK_WIDGET_CLASS (parent_class)->unrealize) (widget);
}

static void port_layout_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GList *tmp_list;
  PortLayout *layout;

  g_return_if_fail (PORT_IS_LAYOUT (widget));

  layout = PORT_LAYOUT (widget);

  requisition->width = layout->requisition.width;
  requisition->height = layout->requisition.height;

  tmp_list = layout->children;

  while (tmp_list)
    {
      GtkWidget *child_widget = tmp_list->data;
      GtkRequisition child_requisition;

      tmp_list = tmp_list->next;

      gtk_widget_size_request(child_widget, &child_requisition);
    }
}

static void port_layout_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  PortLayout *layout;

  g_return_if_fail (PORT_IS_LAYOUT (widget));

  layout = PORT_LAYOUT(widget);
  widget->allocation = *allocation;

  if (GTK_WIDGET_REALIZED (widget))
  {
    gdk_window_move_resize (widget->window,
			      allocation->x, allocation->y,
			      allocation->width, allocation->height);

    gdk_window_resize (layout->bin_window,
			 MAX (layout->width, allocation->width),
			 MAX (layout->height, allocation->height));
  }

  layout->hadjustment->page_size = allocation->width;
  layout->hadjustment->page_increment = allocation->width * 0.9;
  layout->hadjustment->lower = 0;
  /* set_adjustment_upper() emits ::changed */
  port_layout_set_adjustment_upper (layout->hadjustment, MAX (allocation->width, layout->width), TRUE);

  layout->vadjustment->page_size = allocation->height;
  layout->vadjustment->page_increment = allocation->height * 0.9;
  layout->vadjustment->lower = 0;
  layout->vadjustment->upper = MAX (allocation->height, layout->height);
  port_layout_set_adjustment_upper (layout->vadjustment, MAX (allocation->height, layout->height), TRUE);

  handleWindowResize(gtk_widget_get_parent(widget),allocation->width,allocation->height);
  handleContainerReLayout(gtk_widget_get_parent(widget));
}

static gint port_layout_expose (GtkWidget *widget, GdkEventExpose *event)
{
  PortLayout *layout;

  g_return_val_if_fail (PORT_IS_LAYOUT (widget), FALSE);

  layout = PORT_LAYOUT (widget);

  if (event->window != layout->bin_window)
    return FALSE;

  (* GTK_WIDGET_CLASS (parent_class)->expose_event) (widget, event);

  return FALSE;
}

static void port_layout_remove (GtkContainer *container, GtkWidget    *widget)
{
  PortLayout *layout;

  g_return_val_if_fail (PORT_IS_LAYOUT (container), FALSE);

  layout = PORT_LAYOUT (container);

  layout->children = g_list_remove(layout->children, widget);
  gtk_widget_unparent (widget);
}

static void port_layout_forall (GtkContainer *container,
		   gboolean      include_internals,
		   GtkCallback   callback,
		   gpointer      callback_data)
{
  GtkWidget *widget;
  GList *tmp_list;

  g_return_if_fail (PORT_IS_LAYOUT (container));
  g_return_if_fail (callback != NULL);

  tmp_list = PORT_LAYOUT (container)->children;
  while (tmp_list)
    {
      widget = tmp_list->data;
      tmp_list = tmp_list->next;
      (* callback) (widget, callback_data);
    }
}

/* Callbacks */

static void port_layout_adjustment_changed (GtkAdjustment *adjustment, PortLayout *layout)
{
  GtkWidget *widget;
  widget = GTK_WIDGET (layout);

  if (GTK_WIDGET_REALIZED (layout))
    {
      gdk_window_move (layout->bin_window,
		       - layout->hadjustment->value,
		       - layout->vadjustment->value);

      gdk_window_process_updates (layout->bin_window, TRUE);
    }
}
