#include <err.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>

#ifndef PORT_PATH
#define PORT_PATH "/dev/ttyUSB0"
#endif


int main() {

    // See https://stackoverflow.com/questions/6947413/how-to-open-read-and-write-from-serial-port-in-c
    int fd = open(PORT_PATH, O_RDWR | O_NOCTTY | O_SYNC);
    if (-1 == fd) {
        err(-1, "open");
    }


    // set interface attributes
    struct termios tty;
    memset(&tty, 0, sizeof(tty));
    if (tcgetattr(fd, &tty) != 0) {
        err(-1, "tcgetattr");
    }

    cfsetospeed(&tty, B115200);
    cfsetispeed(&tty, B115200);


    // Control modes
    // 8 bit characters
    tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS8;
    // ignore modem controls, enable reading
    tty.c_cflag &= ~(CLOCAL | CREAD);
    // shut off parity
    tty.c_cflag &= ~(PARENB | PARODD);
    // turn off RTS/CTS (hardware) flow control
    tty.c_cflag &= ~CRTSCTS;


    // Input modes
    // receive a SIGINT when a break is read
    tty.c_iflag |= BRKINT;
    // turn off xon/xoff ctrl
    tty.c_iflag &= ~(IXON | IXOFF | IXANY);


    // Local modes
    // no signaling chars, no echo, no canonical processing
    tty.c_lflag = 0;


    // Output modes
    // no remapping, no delays
    tty.c_oflag = 0;


    // Special characters
    // minimum number of characters on read
    tty.c_cc[VMIN] = 0;


    // time in deciseconds for read (read timeout)
    // 0.5 seconds
    tty.c_cc[VTIME] = 5;

    if (0 != tcsetattr(fd, TCSANOW, &tty)) {
        err(-1, "tcsetattr");
    }


    char buf[128];

    int n = read(fd, buf, sizeof(buf));
    printf("%*s\n", n, buf);

    write(fd, "at\r\n", 4);

    n = read(fd, buf, sizeof(buf));
    printf("%*s\n", n, buf);

}
