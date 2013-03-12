#!/usr/bin/python

import threading

def playsound():
	os.system("aplay ~/audio/dream/warning.wav");

 
import sys, time, socket, os
from daemon import daemon
 
class MyDaemon(daemon):
		def run(self):
			s = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
			s.bind(('127.0.0.1', 10102));
			s.listen(1);
			while 1:
				conn, addr = s.accept();
				while 1:
					data = conn.recv(8);
					if not data: break
					if data != b"8atod3pa": break
					else:
						a = threading.Thread(None, playsound);
						a.start();
				conn.close();
 
if __name__ == "__main__":
		daemon = MyDaemon('/tmp/playsound.pid')
		if len(sys.argv) == 2:
				if 'start' == sys.argv[1]:
						daemon.start()
				elif 'stop' == sys.argv[1]:
						daemon.stop()
				elif 'restart' == sys.argv[1]:
						daemon.restart()
				else:
						print("Unknown command");
						sys.exit(2)
				sys.exit(0)
		else:
				print("usage: %s start|stop|restart" % sys.argv[0]);
				sys.exit(2)
