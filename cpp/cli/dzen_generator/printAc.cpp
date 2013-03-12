#include <iostream>
#include <fstream>

using namespace std;

void printAc() {
	static ifstream inputStream;
	static int i = 0;

	inputStream.open("/sys/bus/platform/devices/smapi/ac_connected");
	if (inputStream.good()) {
		i = inputStream.get();
	}
	else {
		cout << "could not open";
	}
	inputStream.close();

	if (i == 48) {
		cout << " NoAC ";
	}
	else {
		cout << "  AC  ";
	}

}
