#!/usr/bin/python
import socket
import sys

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
s.connect(('localhost', 10102));
s.send(b'8atod3pa');
s.close();
