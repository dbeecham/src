#include <stdio.h>
#include <gtk/gtk.h>

int main(int argc, char **argv) {
	if (!gtk_init_check(&argc, &argv)) {
		printf("Could not init GTK.\n");
		return 1;
	}

	GtkWidget *window;
	GtkWidget *hello;
	
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window), 100, 100);
	hello = gtk_label_new("Hello, World!");
	gtk_container_add(GTK_CONTAINER(window), hello);
	
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_widget_show_all(window);
	gtk_main();
	
		
	return 0;
}
