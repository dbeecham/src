#include <sys/utsname.h>
#include <stdio.h>

int main() {
	struct utsname info;
	uname(&info);

	printf("%s\n", info.release);
}
