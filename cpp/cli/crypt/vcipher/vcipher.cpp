#include <iostream>
#include <unistd.h>
#include <stdio.h>

int main(const int args, const char * const argv[]) {
	FILE *fhandle = fopen("./test.txt", "r");
	if (fhandle == NULL) return 1;
	else {
		fclose(fhandle);
	}
}
