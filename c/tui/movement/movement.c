#include <ncurses.h>

struct player {
	int y, x;
};

int main() {
	initscr();
	cbreak();
	noecho();
	curs_set(0);
	nodelay(stdscr, true);
	struct player frightenedScot;
	frightenedScot.y = LINES/2;
	frightenedScot.x = 2;

	mvprintw(frightenedScot.y, frightenedScot.x, ">");
	char c;
	do {
		c = getch();
		if (c != ERR) {
			mvprintw(frightenedScot.y, frightenedScot.x, " ");
			switch (c) {
				case 'j':
					frightenedScot.y++;
					break;
				case 'k':
					frightenedScot.y--;
					break;
				case 'l':
					frightenedScot.x++;
					break;
				case 'h':
					frightenedScot.x--;
					break;
			}
			mvprintw(frightenedScot.y, frightenedScot.x, ">");
		}
	} while (c != 'q');
	endwin();
}
