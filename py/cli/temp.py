#!/usr/bin/python

import commands
import time

while 1:
	print commands.getoutput("cat /proc/acpi/thermal_zone/THRM/temperature")
	time.sleep(1)
