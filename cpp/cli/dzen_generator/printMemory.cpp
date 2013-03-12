#include <iostream>
#include <fstream>
#include <cstring>
#include <stdlib.h>

using namespace std;

int getNumber(char line[]) {
	int i = 0;
	int ni = 0;
	char number[8];
	for (; i != 32; i++) {
		if (line[i] >= '0' && line[i] <= '9') {
			number[ni] = line[i];
			ni++;
		}
	}
	number[ni] = '\0';

	return atoi(number);
}

void printMemory() {
	// Static, I'd like to keep it in memory.
	static ifstream inputStream;
	static char line[32];
	memset(line, 0, 32);

	// Open the file.
	inputStream.open("/proc/meminfo");

	// If it's good...
	if (inputStream.good()) {

		cout.precision(2);

		inputStream.getline(line, 32);
		int total =  getNumber(line);

		inputStream.getline(line, 32);
		int free = getNumber(line);

		inputStream.getline(line, 32);
		int buffers = getNumber(line);

		inputStream.getline(line, 32);
		int cached = getNumber(line);

		cout << " " << fixed << ((float)free/(float)total)*100.0 << "% ";
		cout << "(" << fixed << ((float)buffers/(float)total)*100.0 << "%) ";

	}

	// Could not open the file. :(
	else {
		cout << "could not open";
	}

	// Close the file.
	inputStream.close();
}
