#include <iostream>

using namespace std;

int main() {
	int i [] = {1, 2, 3, 4, 5};

	int *ptr = i;

	cout << *ptr+1;
}
