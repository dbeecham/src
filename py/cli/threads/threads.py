#!/usr/bin/python

import threading

def printone():
	print(1);


a = threading.Thread(None, printone, None);
a.start();
