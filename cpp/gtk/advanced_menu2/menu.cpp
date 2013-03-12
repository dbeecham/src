#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <iostream>

void toggle_statusbar(GtkWidget *statusbar) {
		gtk_widget_hide(statusbar);
}

int main(int argc, char *argv[]) { // {{{
	if (!gtk_init_check(&argc, &argv)) {
		std::cout << "Could not init GTK." << std::endl;
		return 1;
	}

	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *menubar, *viewmenu, *view, *tog_stat, *statusbar; // menubar

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

	vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	menubar = gtk_menu_bar_new();
	viewmenu = gtk_menu_new();

	view = gtk_menu_item_new_with_label("View");
	tog_stat = gtk_check_menu_item_new_with_label("View Statusbar");
	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(tog_stat), TRUE);

	gtk_menu_item_set_submenu(GTK_MENU_ITEM(view), viewmenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(viewmenu), tog_stat);
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar), view);
	gtk_box_pack_start(GTK_BOX(vbox), menubar, FALSE, FALSE, 3);

	statusbar = gtk_statusbar_new();
	gtk_box_pack_end(GTK_BOX(vbox), statusbar, FALSE, TRUE, 1);

	gtk_widget_show_all(window);

	g_signal_connect_swapped(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

	g_signal_connect_swapped(G_OBJECT(tog_stat), "activate", G_CALLBACK(toggle_statusbar), statusbar);

	gtk_main();

	return 0;

} // }}}

// vim: noexpandtab foldmethod=marker
