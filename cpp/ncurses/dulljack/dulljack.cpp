#include <iostream>
#include <ncurses.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>


int main() {
	int sleeptime;
	initscr();
	refresh();
	cbreak();
	timeout(0);

	char string[] = "All work and no play makes jack a dull boy.";
	for (int i = 0; i < LINES; i++) {
		for (int j = 0; j < strlen(string); j++) {
			timeval t;

			if (string[j] >= 'a' && string[j] <= 'z') {
				gettimeofday(&t, NULL);
				srand(t.tv_usec * t.tv_sec);
				if (rand()%6==1) mvaddch(i, j, (char)(string[j]-32));
				else {
					mvaddch(i, j, string[j]);
				}
			}
			else {
				mvaddch(i, j, string[j]);
			}


			gettimeofday(&t, NULL);
			srand(t.tv_usec * t.tv_sec);

			sleeptime = (rand()%10)*50000+50000;

			usleep(sleeptime);
			refresh();

			if (getch() == 'q') {
				endwin();
				return 0;
			}

		}
		usleep(900000);
	}

	endwin();
}
