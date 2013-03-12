#include <iostream>
#include <unistd.h>
#include "functions.h"
#include <getopt.h>

using namespace std;

int main(int argc, char *argv[]) {
	int iterator;
	int longIndex;

	struct GlobalArgs {
		int dzen;
	} globalArgs;
	const char *optString = "d";
	const struct option longOpts[] = {
		{"dzen", no_argument, NULL, 'd'}
	};

	int opt = getopt_long(argc, argv, optString, longOpts, &longIndex);
	while (opt != -1) {
		opt = getopt_long(argc, argv, optString, longOpts, &longIndex);
	}

	while (true) {
		for (iterator = 0; iterator < sizeof(functions)/sizeof(functionPointer); iterator++) {
			functions[iterator]();
			if (iterator+1 != sizeof(functions)/sizeof(functionPointer)) cout << "|";
		}
		cout << endl;
		sleep(1);
	}
}
