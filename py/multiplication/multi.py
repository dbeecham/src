#!/usr/bin/python

import math;
import random;

while 1:
	x = int(math.ceil(random.random()*10)%9+1)
	y = int(math.ceil(random.random()*10)%9+1)
	print str(x) + " * " + str(y)
	i = int(raw_input());
	if i == x*y:
		print "Correct";
	else:
		print "False, correct answer was " + str(x*y);
