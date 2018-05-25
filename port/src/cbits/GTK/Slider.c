#include "Slider.h"
#include "Internals.h"
#include "Handlers_stub.h"

static void slider_adjustment_value_changed_handler (GtkAdjustment *adjustment, gpointer user_data)
{
	handleControlCommand((WindowHandle) user_data);
};

WindowHandle osCreateHorzSlider(WindowHandle window)
{
	GtkWidget *slider;

	slider = gtk_hscale_new_with_range(0, 100, 1);
	gtk_signal_connect (GTK_OBJECT (gtk_range_get_adjustment(GTK_RANGE(slider))), "value-changed",
			GTK_SIGNAL_FUNC(slider_adjustment_value_changed_handler),
			slider);
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), slider);

	return slider;
};

WindowHandle osCreateVertSlider(WindowHandle window)
{
	GtkWidget *slider;

	slider = gtk_vscale_new_with_range(0, 100, 1);
	gtk_signal_connect (GTK_OBJECT (gtk_range_get_adjustment(GTK_RANGE(slider))), "value-changed",
			GTK_SIGNAL_FUNC(slider_adjustment_value_changed_handler),
			slider);
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), slider);

	return slider;
};

void osGetSliderReqSize(WindowHandle slider, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(slider, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}

void osSetSliderRange(WindowHandle slider, int minPos, int maxPos)
{
	GtkAdjustment *adjustment = gtk_range_get_adjustment(GTK_RANGE(slider));
	adjustment->lower = minPos;
	adjustment->upper = maxPos;
	gtk_adjustment_changed(adjustment);
}

void osGetSliderRange(WindowHandle slider, int *minPos, int *maxPos)
{
	GtkAdjustment *adjustment = gtk_range_get_adjustment(GTK_RANGE(slider));
	*minPos = adjustment->lower;
	*maxPos = adjustment->upper;
}

void osSetSliderPosition(WindowHandle slider, int pos)
{
	GtkAdjustment *adjustment = gtk_range_get_adjustment(GTK_RANGE(slider));
	gtk_adjustment_set_value(adjustment, pos);
}

int osGetSliderPosition(WindowHandle slider)
{
	GtkAdjustment *adjustment = gtk_range_get_adjustment(GTK_RANGE(slider));
	return gtk_adjustment_get_value(adjustment);
}
