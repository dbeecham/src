#include <iostream>
#include <ncurses.h>

int main() {
	initscr();
	WINDOW *win = newwin(LINES-4, COLS-4, 2, 2); 
	box(win, 0, 0);
	refresh();
	wrefresh(win);
	getch();
	endwin();
}
