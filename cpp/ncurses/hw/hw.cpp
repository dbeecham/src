#include <iostream>
#include <ncurses.h>

int main() {
	initscr();
	printw("Hello, World.");
	getch();
	endwin();
}
