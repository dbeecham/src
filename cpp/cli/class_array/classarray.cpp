#include <iostream>

using namespace std;

class test {
	public:
		test();
};

test::test() {
	cout << "Called!" << endl;
}

int main() {
	test testos[3];
}
