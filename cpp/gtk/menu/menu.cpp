#include <gtk/gtk.h>
#include <iostream>

int main(int args, char *argv[]) { // {{{
	if (!(gtk_init_check(&args, &argv))) {
		std::cout << "Could not init GTK." << std::endl;
		return 1;
	}

	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *menubar, *filemenu, *file, *quit; // the menu bar

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window), 300, 200);
	gtk_window_set_title(GTK_WINDOW(window), "Hello world!");

	vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	menubar = gtk_menu_bar_new();
	filemenu = gtk_menu_new();

	file = gtk_menu_item_new_with_label("File");
	quit = gtk_menu_item_new_with_label("Quit");

	gtk_menu_item_set_submenu(GTK_MENU_ITEM(file), filemenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu), quit);
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar), file);
	gtk_box_pack_start(GTK_BOX(vbox), menubar, FALSE, FALSE, 3);

	gtk_widget_show_all(window);
	gtk_main();
	return 0;
} // }}}
