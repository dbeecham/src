#include <netinet/in.h>
#include <stdio.h>

void *netupdate(void *args) {
	
	// Get arg.
	class everything *e = (class everything *)args;

	// Set up variables.
	int sock, remote_sock;
	struct sockaddr_in in_saddr, remote_in_saddr;
	char buffer[128];

	// Set up networking.
	in_saddr.sin_family = AF_INET;
	in_saddr.sin_addr.s_addr = INADDR_ANY;
	in_saddr.sin_port = htons(23000);

	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		printf("Could not set up socket.\n");
		return;
	}

	if (bind(sock, (struct sockaddr *)&sin_saddr, sizeof(sin_addr)) < 0) {
		printf("Could not bind to socket.\n");
		return;
	}

	listen(sock, 5);

	while (1) {
		int remote_in_saddr_size = sizeof(remote_in_saddr);
	}
}
