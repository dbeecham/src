#include <stdio.h>
#include <string.h>

namespace test {
	char* func() {
		char *str = new char[sizeof("hello")];
		strcpy(str, "hello");
		return str;
	}
}

int main() {
	printf("%s\n", test::func());
}
