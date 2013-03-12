#include <stdio.h>

int main() {
	int arr[8] = {1,2,3,4,5,6,7,8};
	void *ptr;
	ptr = arr;

	++*(int*)ptr;

	int i;
	for (i = 0; i < sizeof(arr)/sizeof(int); i++) {
		printf("%d\n", arr[i]);
	}
}
