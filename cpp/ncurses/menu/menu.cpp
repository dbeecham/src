#include <stdio.h>
#include <menu.h>
#include <ncurses.h>
#include <string.h>

class curs {
	private:
		WINDOW *mainwin;
		WINDOW *input;
		WINDOW *output;
		int output_height;
		int output_width;
		int output_cur_y;

	public:
		curs();
		~curs();
		void refresh();
		void get_input(char buf[]);
		void print(char out[]);
};

curs::curs() { // Constructor {{{

	output_cur_y = 0;

	// Set up main window
	mainwin = initscr();

	// Set up output window
	output_height = LINES/2;
	output_width = COLS/2;
	output = newwin(output_height, output_width, (LINES/2)-(output_height/2), (COLS/2)-(output_width/2));
	scrollok(output, true);


	// Set up input window
	input = newwin(3, output_width, ((LINES/2)+(output_height/2))+3, (COLS/2)-(output_width/2));

	// Pretty it up a bit
	box(output, 0, 0);
	box(input, 0, 0);

} // }}}

curs::~curs() { // Destructor {{{
	endwin();
} // }}}

void curs::refresh() { // {{{
	wrefresh(mainwin);
	wrefresh(output);
	box(input, 0, 0);
	wrefresh(input);
} // }}}

void curs::get_input(char buf[]) { // {{{
	mvwscanw(input, 1, 1, "%128s", buf);
	wclear(input);
} // }}}

void curs::print(char out[]) { // {{{
	if (output_cur_y == output_height-2) {
		wborder(output, ' ', ' ', ' ',' ',' ',' ',' ',' ');
		scroll(output);
		box(output, 0, 0);
	}
	else output_cur_y++;

	mvwprintw(output, output_cur_y, 1, "%s", out);
} // }}}

int main() {
	curs mainwin;
	char buf[128];

	while (strcmp(buf, "quit") != 0 ) {
		mainwin.refresh();
		mainwin.get_input(buf);
		mainwin.print(buf);
	}
}
