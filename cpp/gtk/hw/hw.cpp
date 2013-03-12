#include <gtk/gtk.h>
#include <iostream>

int main(int args, char *argv[]) { // {{{
	GtkWidget *window;

	if (!gtk_init_check(&args, &argv)) {
		std::cout << "Couldn't init GTK." << std::endl;
	}

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "Hello, World!");
	gtk_window_set_default_size(GTK_WINDOW(window), 200, 300);
	gtk_widget_show(window);

	gtk_main();
	return 0;
} // }}}
