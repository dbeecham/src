#include <ncurses.h>
#include <math.h>
#include <getopt.h>
#include <stdlib.h>
#include <stdio.h>

struct enemy_t {
	int x;
	int y;
	struct enemy_t *next;
	struct enemy_t *prev;
};

void new_enemy(void *, void *, int, int);

int main(int argc, char **argv) {
	typedef struct enemy_t enemy;
	enemy *cur, *head;

	srand(time(NULL));

	int options = 0;
	// 1 == verbose
	// 2 == box
	// command line options {{{
	static struct option long_options[] = {
		{"version", 0, 0, 'v'},
		{"verbose", 0, 0, 'V'},
		{"box", 0, 0, 'b'},
		{"help", 0, 0, 'h'},
		{0, 0, 0, 0}
	};

	int option_index;
	char c;

	while (1) {
		c = getopt_long(argc, argv, "hbVv", long_options, &option_index);
		if (c == EOF) break;
		if (c == 'v') {
			printf("sinuscurve version 0.0.0\n");
			return 0;
		}
		if (c == 'V') {
			printf("verbose mode on.\n");
			options += 1;
		}
		if (c == 'b')
			options += 2;
		if (c == 'h') {
			printf("help?\n");
			return 0;
		}
	}
	// }}}
	if (options&1 == 1)
		printf("running...\n");

	initscr();
	curs_set(0);
	
	if ((options&2) > 0)
		box(stdscr, 0, 0);

	int i = 10;
	head = NULL;
	while (1) {
		if (i == 10) {
			cur = (enemy *)malloc(sizeof(enemy));
			cur->y = LINES/2;
			cur->y = rand()%COLS;
			if ((options&2) > 0)
				cur->x = COLS-2;
			else
				cur->x = COLS-1;
			cur->next = head;
			if (cur->next) cur->next->prev = cur;
			head = cur;
			i = 0;
		}
		i++;

		cur = head;
		while (cur) {
			mvprintw(cur->y, cur->x, " ");
			cur->x--;
			mvprintw(cur->y, cur->x, "#");
			cur = cur->next;
		}

		refresh();
		usleep(25000);
	}

	if (options&1 == 1)
		printf("everything seems to have gone right, exiting now.\n");
	endwin();

	return 0;
}
