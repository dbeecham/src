#include <stdio.h>
#include <string.h>
#include <netinet/in.h>

int main(int argc, char **argv) {
	int sock, newsock;
	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) return 2;

	struct sockaddr_in addr;
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = INADDR_ANY;
	addr.sin_port = htons(10102);

	if (connect(sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) return 3;

	if (send(sock, "hi", 2, 0) < 2) return 4;

	return 0;
}
