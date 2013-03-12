#include <stdio.h>

struct menu {
	char *entry;
	struct menu *next;
	int (*func)(); // Link to a function with no argument.
};

int printstuff() {
	printf("testostost");
}

int main() {

	struct menu mainmenu;
	mainmenu.func = printstuff;
	mainmenu.func();

}
