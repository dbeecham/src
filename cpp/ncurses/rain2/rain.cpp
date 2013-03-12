#include <ncurses.h>
#include <sys/time.h>
#include <stdlib.h>
#include <unistd.h>

class raindrop { // {{{
	public:
		int lastx;
		int lasty;
		int followers;

		int x;
		int y;

		raindrop();
		void run();
		void newpos();
};

raindrop::raindrop() {
	newpos();
	lasty = y;
	lastx = x;
}

void raindrop::newpos() {
	timeval t;
	gettimeofday(&t, NULL);
	srand(t.tv_usec * t.tv_sec);
	x = rand()%COLS;

	gettimeofday(&t, NULL);
	srand(t.tv_usec * t.tv_sec);
	y = rand()%9;
}

void raindrop::run() {
	if (y >= LINES) newpos();
	mvprintw(y, x, "#");
	y++;
}
// }}}

int main() {
	
	// Init ncurses
	initscr();
	cbreak();
	timeout(0);

	// Set up some raindrops.
	raindrop rain[5];

	// Main loop!
	while (true) {
	
		// If q were pressed, end this loop.
		if (getch() == 'q') {
			break;
		}

		// Update raindrops.
		for (int i = 0; i < sizeof(rain)/sizeof(raindrop); i++) {
			rain[i].run();
		}

		// Refresh screen.
		refresh();

		// Small break.
		usleep(100000);

	}
	
	// End ncurses.
	endwin();
}
