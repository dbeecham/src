#include <iostream>

using namespace std;

int main(int const argc, char const * const * const argv) {
	char const * const * argvptr = argv;
	cout << *argvptr << endl;
	cout << *++argvptr << endl;
}
