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

	if (bind(sock, (struct sockaddr*) &addr, sizeof(addr)) < 0) return 3;

	listen (sock, 5);

	struct sockaddr_in inc_addr;
	int inc_addr_size = sizeof(inc_addr);
	if ((newsock = accept(sock, (struct sockaddr*) &inc_addr, &inc_addr_size)) < 0) return 4;

	char buffer[256];
	int rtrn;
	memset(buffer, 0, sizeof(buffer));
	if (read(newsock, buffer, 255) < 0) return 5;
	else printf("%s\n", buffer);

	return 0;
}
