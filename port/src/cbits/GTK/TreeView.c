#include "TreeView.h"
#include "Internals.h"
#include "Handlers_stub.h"


#define PORT_TYPE_TREE_STORE            (port_tree_store_get_type ())
#define PORT_TREE_STORE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), PORT_TYPE_TREE_STORE, PortTreeStore))
#define PORT_TREE_STORE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  PORT_TYPE_TREE_STORE, PortTreeStoreClass))
#define PORT_IS_TREE_STORE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), PORT_TYPE_TREE_STORE))
#define PORT_IS_TREE_STORE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  PORT_TYPE_TREE_STORE))
#define PORT_TREE_STORE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  PORT_TYPE_TREE_STORE, PortTreeStoreClass))

typedef struct _PortRecord          PortRecord;
typedef struct _PortTreeStore       PortTreeStore;
typedef struct _PortTreeStoreClass  PortTreeStoreClass;



/* PortRecord: this structure represents a row */

struct _PortRecord
{
	guint       pos;   /* pos within the children */
	PortRecord* next;
	PortRecord* parent;
	guint        num_children;
	PortRecord  *first_child;
	PortRecord  *last_child;
};

struct _PortTreeStore
{
	GObject parent;         /* this MUST be the first member */

	GtkWidget* view;        /* we can have only one view */
	PortRecord root;

	/* These two fields are not absolutely necessary, but they    */
	/*   speed things up a bit in our get_value implementation    */
	gint   n_columns;
	GType* column_types;

	gint stamp;             /* Random integer to check whether an iter belongs to our model */
};



/* PortTreeStoreClass: more boilerplate GObject stuff */

struct _PortTreeStoreClass
{
	GObjectClass parent_class;
};


static GType        port_tree_store_get_type (void);

static void         port_tree_store_init            (PortTreeStore      *pkg_tree);

static void         port_tree_store_class_init      (PortTreeStoreClass *klass);

static void         port_tree_store_tree_model_init (GtkTreeModelIface *iface);

static void         port_tree_store_finalize        (GObject           *object);

static GtkTreeModelFlags port_tree_store_get_flags  (GtkTreeModel      *tree_model);

static gint         port_tree_store_get_n_columns   (GtkTreeModel      *tree_model);

static GType        port_tree_store_get_column_type (GtkTreeModel      *tree_model,
                                                 gint               index);

static gboolean     port_tree_store_get_iter    (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter,
                                                 GtkTreePath       *path);

static GtkTreePath *port_tree_store_get_path    (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter);

static void         port_tree_store_get_value   (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter,
                                                 gint               column,
                                                 GValue            *value);

static gboolean     port_tree_store_iter_next   (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter);

static gboolean     port_tree_store_iter_children
                                                (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter,
                                                 GtkTreeIter       *parent);

static gboolean     port_tree_store_iter_has_child
                                                (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter);

static gint         port_tree_store_iter_n_children
                                                (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter);

static gboolean     port_tree_store_iter_nth_child 
                                                (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter,
                                                 GtkTreeIter       *parent,
                                                 gint               n);

static gboolean     port_tree_store_iter_parent
                                                (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter,
                                                 GtkTreeIter       *child);



static GObjectClass *parent_class = NULL;  /* GObject stuff - nothing to worry about */


/*****************************************************************************
 *
 *  port_tree_store_get_type: here we register our new type and its interfaces
 *                        with the type system. If you want to implement
 *                        additional interfaces like GtkTreeSortable, you
 *                        will need to do it here.
 *
 *****************************************************************************/

static GType
port_tree_store_get_type (void)
{
  static GType port_tree_store_type = 0;

  if (port_tree_store_type)
    return port_tree_store_type;

  /* Some boilerplate type registration stuff */
  if (1)
  {
    static const GTypeInfo port_tree_store_info =
    {
      sizeof (PortTreeStoreClass),
      NULL,                                         /* base_init */
      NULL,                                         /* base_finalize */
      (GClassInitFunc) port_tree_store_class_init,
      NULL,                                         /* class finalize */
      NULL,                                         /* class_data */
      sizeof (PortTreeStore),
      0,                                           /* n_preallocs */
      (GInstanceInitFunc) port_tree_store_init
    };

    port_tree_store_type = g_type_register_static (G_TYPE_OBJECT, "PortTreeStore",
                                                   &port_tree_store_info, (GTypeFlags)0);
  }

  /* Here we register our GtkTreeModel interface with the type system */
  if (1)
  {
    static const GInterfaceInfo tree_model_info =
    {
      (GInterfaceInitFunc) port_tree_store_tree_model_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (port_tree_store_type, GTK_TYPE_TREE_MODEL, &tree_model_info);
  }

  return port_tree_store_type;
}


/*****************************************************************************
 *
 *  port_tree_store_class_init: more boilerplate GObject/GType stuff.
 *                          Init callback for the type system,
 *                          called once when our new class is created.
 *
 *****************************************************************************/

static void
port_tree_store_class_init (PortTreeStoreClass *klass)
{
	GObjectClass *object_class;

	parent_class = (GObjectClass*) g_type_class_peek_parent (klass);
	object_class = (GObjectClass*) klass;

	object_class->finalize = port_tree_store_finalize;
}

/*****************************************************************************
 *
 *  port_tree_store_tree_model_init: init callback for the interface registration
 *                               in port_tree_store_get_type. Here we override
 *                               the GtkTreeModel interface functions that
 *                               we implement.
 *
 *****************************************************************************/

static void
port_tree_store_tree_model_init (GtkTreeModelIface *iface)
{
	iface->get_flags       = port_tree_store_get_flags;
	iface->get_n_columns   = port_tree_store_get_n_columns;
	iface->get_column_type = port_tree_store_get_column_type;
	iface->get_iter        = port_tree_store_get_iter;
	iface->get_path        = port_tree_store_get_path;
	iface->get_value       = port_tree_store_get_value;
	iface->iter_next       = port_tree_store_iter_next;
	iface->iter_children   = port_tree_store_iter_children;
	iface->iter_has_child  = port_tree_store_iter_has_child;
	iface->iter_n_children = port_tree_store_iter_n_children;
	iface->iter_nth_child  = port_tree_store_iter_nth_child;
	iface->iter_parent     = port_tree_store_iter_parent;
}


/*****************************************************************************
 *
 *  port_tree_store_init: this is called everytime a new port tree store
 *                        instance is created. Initialise the tree structure's
 *                        fields here.
 *
 *****************************************************************************/

static void
port_tree_store_init (PortTreeStore *port_tree_store)
{
	port_tree_store->n_columns = 0;
	port_tree_store->column_types = NULL;
	port_tree_store->view = NULL;
	port_tree_store->root.parent = NULL;
	port_tree_store->root.next = NULL;
	port_tree_store->root.pos = 0;
	port_tree_store->root.num_children = 0;
	port_tree_store->root.first_child = NULL;
	port_tree_store->root.last_child = NULL;

	port_tree_store->stamp = g_random_int();  /* Random int to check whether an iter belongs to our model */
}


/*****************************************************************************
 *
 *  port_tree_store_finalize: this is called just before a custom list is
 *                        destroyed. Free dynamically allocated memory here.
 *
 *****************************************************************************/

static void
port_tree_release_children(PortRecord *record)
{
	record = record->first_child;
	while (record != NULL) {
		PortRecord *next = record->next;
		port_tree_release_children(record);
		free(record);
		record = next;
	}
}

static void
port_tree_store_finalize (GObject *object)
{
  PortTreeStore *port_tree_store = PORT_TREE_STORE(object);

  /* free all records and free all memory used by the list */
  port_tree_release_children(&port_tree_store->root);
  free(port_tree_store->column_types);

  /* must chain up - finalize parent */
  (* parent_class->finalize) (object);
}


/*****************************************************************************
 *
 *  port_tree_store_get_flags: tells the rest of the world whether our tree model
 *                         has any special characteristics. In our case,
 *                         we have a list model (instead of a tree), and each
 *                         tree iter is valid as long as the row in question
 *                         exists, as it only contains a pointer to our struct.
 *
 *****************************************************************************/

static GtkTreeModelFlags
port_tree_store_get_flags (GtkTreeModel *tree_model)
{
	g_return_val_if_fail (PORT_IS_TREE_STORE(tree_model), (GtkTreeModelFlags)0);

	return (GTK_TREE_MODEL_ITERS_PERSIST);
}


/*****************************************************************************
 *
 *  port_tree_store_get_n_columns: tells the rest of the world how many data
 *                             columns we export via the tree model interface
 *
 *****************************************************************************/

static gint
port_tree_store_get_n_columns (GtkTreeModel *tree_model)
{
	g_return_val_if_fail (PORT_IS_TREE_STORE(tree_model), 0);

	return PORT_TREE_STORE(tree_model)->n_columns;
}


/*****************************************************************************
 *
 *  port_tree_store_get_column_type: tells the rest of the world which type of
 *                               data an exported model column contains
 *
 *****************************************************************************/

static GType
port_tree_store_get_column_type (GtkTreeModel *tree_model,
                                 gint          index)
{
	g_return_val_if_fail (PORT_IS_TREE_STORE(tree_model), G_TYPE_INVALID);
	g_return_val_if_fail (index < PORT_TREE_STORE(tree_model)->n_columns && index >= 0, G_TYPE_INVALID);

	return PORT_TREE_STORE(tree_model)->column_types[index];
}


/*****************************************************************************
 *
 *  port_tree_store_get_iter: converts a tree path (physical position) into a
 *                        tree iter structure (the content of the iter
 *                        fields will only be used internally by our model).
 *                        We simply store a pointer to our PortRecord
 *                        structure that represents that row in the tree iter.
 *
 *****************************************************************************/

static gboolean
port_tree_store_get_iter (GtkTreeModel *tree_model,
                          GtkTreeIter  *iter,
                          GtkTreePath  *path)
{
	PortRecord *record;
	PortTreeStore *port_tree_store;
	gint *indices, n, depth, i;

	g_assert(PORT_IS_TREE_STORE(tree_model));
	g_assert(path!=NULL);

	port_tree_store = PORT_TREE_STORE(tree_model);

	indices = gtk_tree_path_get_indices(path);
	depth   = gtk_tree_path_get_depth(path);

	record = &port_tree_store->root;
	for (i = 0; i < depth; i++) {
		n = indices[i];
		if (n >= record->num_children || n < 0)
			return FALSE;

		record = record->first_child;
		if (record == NULL)
			return FALSE;

		while (n > 0) {
			record = record->next; n--;
			if (record == NULL)
				return FALSE;
		}
	}

	/* We simply store a pointer to our custom record in the iter */
	iter->stamp      = port_tree_store->stamp;
	iter->user_data  = record;
	iter->user_data2 = NULL;   /* unused */
	iter->user_data3 = NULL;   /* unused */

	return TRUE;
}


/*****************************************************************************
 *
 *  port_tree_store_get_path: converts a tree iter into a tree path.
 *
 *****************************************************************************/

static GtkTreePath *
port_tree_store_get_path (GtkTreeModel *tree_model,
                          GtkTreeIter  *iter)
{
	GtkTreePath  *path;
	PortRecord *record;
	PortTreeStore   *port_tree_store;

	g_return_val_if_fail (PORT_IS_TREE_STORE(tree_model), NULL);
	g_return_val_if_fail (iter != NULL,               NULL);
	g_return_val_if_fail (iter->user_data != NULL,    NULL);

	port_tree_store = PORT_TREE_STORE(tree_model);

	record = (PortRecord*) iter->user_data;

	path = gtk_tree_path_new();
	while (record != &port_tree_store->root) {
		gtk_tree_path_prepend_index(path, record->pos);
		record = record->parent;
	}

	return path;
}


/*****************************************************************************
 *
 *  port_tree_store_get_value: Returns a row's exported data columns
 *                         (_get_value is what gtk_tree_model_get uses)
 *
 *****************************************************************************/

static void
port_tree_store_get_value (GtkTreeModel *tree_model,
                           GtkTreeIter  *iter,
                           gint          column,
                           GValue       *value)
{
	PortRecord *record;
	PortTreeStore *port_tree_store;

	g_return_if_fail (PORT_IS_TREE_STORE (tree_model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (column < PORT_TREE_STORE(tree_model)->n_columns);

	record = (PortRecord*) iter->user_data;
	g_return_if_fail (record != NULL);

	port_tree_store = PORT_TREE_STORE(tree_model);

	g_value_init (value, port_tree_store->column_types[column]);

	if (record->pos >= port_tree_store->root.num_children)
		g_return_if_reached();

	if (port_tree_store->column_types[column] == G_TYPE_INT) {
		int n;
		if (handleTreeViewGetter(port_tree_store->view, record, column, &n)) {
			g_value_set_int(value, n);
		}
	} else if (port_tree_store->column_types[column] == G_TYPE_STRING) {
		char* s;
		if (handleTreeViewGetter(port_tree_store->view, record, column, &s)) {
			g_value_set_string(value, s);
			free(s);
		}
	}
}


/*****************************************************************************
 *
 *  port_tree_store_iter_next: Takes an iter structure and sets it to point
 *                         to the next row.
 *
 *****************************************************************************/

static gboolean
port_tree_store_iter_next (GtkTreeModel  *tree_model,
                           GtkTreeIter   *iter)
{
	PortRecord  *record;
	PortTreeStore    *port_tree_store;

	g_return_val_if_fail (PORT_IS_TREE_STORE (tree_model), FALSE);

	if (iter == NULL || iter->user_data == NULL)
		return FALSE;

	port_tree_store = PORT_TREE_STORE(tree_model);

	record = (PortRecord *) iter->user_data;

	/* Is this the last record in the list? */
	if (record->next == NULL)
		return FALSE;

	iter->stamp     = port_tree_store->stamp;
	iter->user_data = record->next;

	return TRUE;
}


/*****************************************************************************
 *
 *  port_tree_store_iter_children: Returns TRUE or FALSE depending on whether
 *                             the row specified by 'parent' has any children.
 *                             If it has children, then 'iter' is set to
 *                             point to the first child.
 *
 *****************************************************************************/

static gboolean
port_tree_store_iter_children (GtkTreeModel *tree_model,
                               GtkTreeIter  *iter,
                               GtkTreeIter  *parent)
{
	PortRecord  *record;
	PortTreeStore  *port_tree_store;

	g_return_val_if_fail (PORT_IS_TREE_STORE (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);

	port_tree_store = PORT_TREE_STORE(tree_model);

	if (parent == NULL)
		record = &port_tree_store->root;
	else {
		record = (PortRecord *) parent->user_data;
		g_assert(record != NULL);
	}

	/* No rows => no first row */
	if (record->first_child == NULL)
		return FALSE;

	/* Set iter to first item in list */
	iter->stamp     = port_tree_store->stamp;
	iter->user_data = record->first_child;

	return TRUE;
}


/*****************************************************************************
 *
 *  port_tree_store_iter_has_child: Returns TRUE or FALSE depending on whether
 *                              the row specified by 'iter' has any children.
 *
 *****************************************************************************/

static gboolean
port_tree_store_iter_has_child (GtkTreeModel *tree_model,
                                GtkTreeIter  *iter)
{
	PortRecord  *record;
	PortTreeStore  *port_tree_store;

	g_return_val_if_fail (PORT_IS_TREE_STORE (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);

	port_tree_store = PORT_TREE_STORE(tree_model);

	if (iter == NULL)
		record = &port_tree_store->root;
	else
		record = (PortRecord *) iter->user_data;

	/* No rows => no first row */
	return (record->first_child != NULL);
}


/*****************************************************************************
 *
 *  port_tree_store_iter_n_children: Returns the number of children the row
 *                               specified by 'iter' has.
 *
 *****************************************************************************/

static gint
port_tree_store_iter_n_children (GtkTreeModel *tree_model,
                                 GtkTreeIter  *iter)
{
	PortRecord  *record;
	PortTreeStore  *port_tree_store;

	g_return_val_if_fail (PORT_IS_TREE_STORE (tree_model), -1);

	port_tree_store = PORT_TREE_STORE(tree_model);

	if (iter == NULL)
		record = &port_tree_store->root;
	else {
		record = (PortRecord *) iter->user_data;
		g_assert (record != NULL);
	}

	return record->num_children;
}


/*****************************************************************************
 *
 *  port_tree_store_iter_nth_child: If the row specified by 'parent' has any
 *                              children, set 'iter' to the n-th child and
 *                              return TRUE if it exists, otherwise FALSE.
 *
 *****************************************************************************/

static gboolean
port_tree_store_iter_nth_child (GtkTreeModel *tree_model,
                                GtkTreeIter  *iter,
                                GtkTreeIter  *parent,
                                gint          n)
{
	PortRecord  *record;
	PortTreeStore    *port_tree_store;

	g_return_val_if_fail(PORT_IS_TREE_STORE (tree_model), FALSE);

	port_tree_store = PORT_TREE_STORE(tree_model);

	if (parent == NULL)
		record = &port_tree_store->root;
	else {
		record = (PortRecord *) parent->user_data;
		g_assert (record != NULL);
	}

	/* special case: if parent == NULL, set iter to n-th top-level row */
	if (n >= record->num_children)
		return FALSE;

	record = record->first_child;
	while (n > 0) {
	  record = record->next; n--;
	}

	g_assert(record != NULL);

	iter->stamp = port_tree_store->stamp;
	iter->user_data = record;

	return TRUE;
}


/*****************************************************************************
 *
 *  port_tree_store_iter_parent: Point 'iter' to the parent node of 'child'.
 *
 *****************************************************************************/

static gboolean
port_tree_store_iter_parent (GtkTreeModel *tree_model,
                             GtkTreeIter  *iter,
                             GtkTreeIter  *child)
{
	PortRecord  *record;
	PortTreeStore    *port_tree_store;

	g_return_val_if_fail(PORT_IS_TREE_STORE (tree_model), FALSE);
	g_return_val_if_fail(iter != NULL && child != NULL, FALSE);

	port_tree_store = PORT_TREE_STORE(tree_model);

	record = (PortRecord *) child->user_data;
	if (record == NULL)
		return FALSE;
	if (record->parent == NULL ||
	    record->parent == &port_tree_store->root)
		return FALSE;

	iter->stamp = port_tree_store->stamp;
	iter->user_data = record->parent;

	return TRUE;
}

WindowHandle osCreateTreeView(WindowHandle window)
{
	PortTreeStore  *model;
	GtkWidget      *view;

	model = (PortTreeStore*) g_object_new (PORT_TYPE_TREE_STORE, NULL);
	view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(model));
	model->view = view;
	g_object_unref(model); /* destroy store automatically with view */

	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), view);

	return view;
}

int osAddTreeViewColumn(WindowHandle treeview, PortString title, int type)
{
	PortTreeStore* port_tree_store =
		PORT_TREE_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(treeview)));

	gint index = port_tree_store->n_columns++;
	port_tree_store->column_types =
		g_realloc(port_tree_store->column_types,
		          port_tree_store->n_columns*sizeof(GType*));
	switch (type) {
	case 1:
		port_tree_store->column_types[index] = G_TYPE_INT;
		break;
	case 2:
		port_tree_store->column_types[index] = G_TYPE_STRING;
		break;
	}

	GtkTreeViewColumn* col = gtk_tree_view_column_new();
	GtkCellRenderer* renderer = gtk_cell_renderer_text_new();

	gtk_tree_view_column_pack_start (col, renderer, TRUE);
	gtk_tree_view_column_add_attribute (col, renderer, "text", index);
	gtk_tree_view_column_set_title (col, title);
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview),col);

	return index;
}

RowHandle osAppendTreeViewItem (WindowHandle treeview, RowHandle parent)
{
	GtkTreeIter   iter;
	GtkTreePath  *path;
	PortRecord *newrecord;

	PortTreeStore* port_tree_store =
		PORT_TREE_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(treeview)));

	if (parent == NULL)
		parent = &port_tree_store->root;

	newrecord = malloc(sizeof(PortRecord));
	newrecord->pos = parent->num_children++;
	newrecord->next = NULL;
	newrecord->parent = parent;
	newrecord->num_children = 0;
	newrecord->first_child = NULL;
	newrecord->last_child = NULL;

	if (parent->last_child != NULL) {
		parent->last_child->next = newrecord;
	} else {
		parent->first_child = newrecord;
	}
	parent->last_child = newrecord;

	/* inform the tree view and other interested objects
	 *  (e.g. tree row references) that we have inserted
	 *  a new row, and where it was inserted */

	iter.stamp      = port_tree_store->stamp;
	iter.user_data  = newrecord;
	iter.user_data2 = NULL;   /* unused */
	iter.user_data3 = NULL;   /* unused */

	path = port_tree_store_get_path(GTK_TREE_MODEL(port_tree_store), &iter);
	gtk_tree_model_row_inserted(GTK_TREE_MODEL(port_tree_store), path, &iter);
	gtk_tree_path_free(path);

	return newrecord;
}

void osGetTreeViewReqSize(WindowHandle treeview, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(treeview, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}
