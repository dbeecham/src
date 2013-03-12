#include <iostream>

using namespace std;

struct list {
	int i;
	list *next;
};

int main() {
	list ll;

	if (ll.next == NULL)
		cout << "null" << endl;

	ll.next = new list;

	if (ll.next == NULL)
		cout << "null" << endl;
	else
		cout << "not null" << endl;
}
