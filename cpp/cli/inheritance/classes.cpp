#include <string.h>
#include <iostream>

using namespace std;

class mother {
	public:
		char name[128];
		mother();
};

mother::mother() {
	strcpy(name, "abcdef");
	cout << "Called mother constructor." << endl;
}

class child: public mother {
	public:
		int jah;

		child();
};

child::child() {
	strcpy(name, "123");
	cout << "Called child constructor." << endl;
}
