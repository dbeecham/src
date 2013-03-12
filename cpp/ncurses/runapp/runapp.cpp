#include <ncurses.h>

int main() {
	// Variables for later use
	WINDOW *winput, *woutput;
	char buf[128];
	int i;

	// Set up ncurses
	initscr();
	winput = newwin(2, COLS, LINES-2, 0);
	woutput = newwin(5, COLS, 10, 10);
	scrollok(woutput, TRUE);



	// Main loop
	while (i < 10) {
		// Get some input
		mvwscanw(winput, 1, 1, "%128s", buf);

		// Print out the input
		wprintw(woutput, "%s\n", buf);
		
		// Scroll output window
		scroll(woutput);
		wclrtoeol(woutput);
		
		// Refresh
		wrefresh(winput);
		wrefresh(woutput);

		i++;
	}
	
	// End ncurses
	endwin();
}
