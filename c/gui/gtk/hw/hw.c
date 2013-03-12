//      hw.c
//      
//      Copyright 2009 Daniel Beecham <joshu@lunix.se>
//      
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 2 of the License, or
//      (at your option) any later version.
//      
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.


#include <stdio.h>
#include <gtk/gtk.h>

int main(int argc, char** argv)
{
	if (!(gtk_init_check(&argc, &argv))) {
		printf("Could not init GTK.");
		return 1;
	}
	
	GtkWidget *window;
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "Hello, world!");
	gtk_window_set_default_size(GTK_WINDOW(window), 300, 300);
	gtk_widget_show_all(window);
	
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_main();
	
	return 0;
}
