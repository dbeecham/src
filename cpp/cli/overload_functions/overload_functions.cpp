#include <iostream>

using namespace std;

void test(int i) {
	cout << "int: " << i+i;
}

void test(char i) {
	cout << "char: " << i;
}

int main() {
	test(3.2);
	cout << endl;
	return 0;
}
