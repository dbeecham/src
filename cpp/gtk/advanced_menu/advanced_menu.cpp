#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <iostream>

int main(int args, char *argv[]) {
		if (!(gtk_init_check(&args, &argv))) {
			std::cout << "Could not init GTK!" << std::endl;
			return 1;
		}

		GtkWidget *window;
		GtkWidget *vbox;
		GtkWidget *menubar, *filemenu, *file, *newfile, *open, *quit; // menu
		GtkAccelGroup *accelGroup = NULL;

		window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
		gtk_window_set_default_size(GTK_WINDOW(window), 200, 300);
		gtk_window_set_title(GTK_WINDOW(window), "advanced menu");

		vbox = gtk_vbox_new(FALSE, 0);
		gtk_container_add(GTK_CONTAINER(window), vbox);

		menubar = gtk_menu_bar_new();
		filemenu = gtk_menu_new();
		accelGroup = gtk_accel_group_new();
		gtk_window_add_accel_group(GTK_WINDOW(window), accelGroup);

		file = gtk_menu_item_new_with_mnemonic("_File");
		newfile = gtk_image_menu_item_new_from_stock(GTK_STOCK_NEW, NULL);
		open = gtk_image_menu_item_new_from_stock(GTK_STOCK_OPEN, NULL);
		quit = gtk_image_menu_item_new_from_stock(GTK_STOCK_QUIT, NULL);

		gtk_widget_add_accelerator(quit, "activate", accelGroup, GDK_Q, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);

		gtk_menu_item_set_submenu(GTK_MENU_ITEM(file), filemenu);
		gtk_menu_shell_append(GTK_MENU_SHELL(filemenu), newfile);
		gtk_menu_shell_append(GTK_MENU_SHELL(filemenu), open);
		gtk_menu_shell_append(GTK_MENU_SHELL(filemenu), quit);
		gtk_menu_shell_append(GTK_MENU_SHELL(menubar), file);
		gtk_box_pack_start(GTK_BOX(vbox), menubar, FALSE, FALSE, 3);

		gtk_widget_show_all(window);

		gtk_main();
		return 0;

}
