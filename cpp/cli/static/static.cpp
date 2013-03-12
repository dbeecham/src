#include <stdio.h>

bool statictest() {
	static bool init = false;
	
	// First time around it'll toggle init to true
	if(!init) init = !init;

	// Second time around it'll return false.
	else return false;

	// First time around, again, it'll return true.
	return true;
}

int main() {
	
	// Loop until statictest returns false, which is the second time.
	while (statictest()) {}

	// Then return 0.
	return 0;
}
