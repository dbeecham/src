#!/usr/bin/python

ICONPATH="/home/zhaozhou/.dzen2/icons/xbm8x8"

if __name__ == "__main__":

	from datetime import datetime
	import time as ctime
	import re
	import subprocess
	import os
	from dateutil.relativedelta import *
	
	
	# AC connected

	handle = open("/sys/bus/platform/devices/smapi/ac_connected", "r");
	ac = handle.read();
	handle.close();

	if (ac[0] == "1"): print("^i(%s/ac_01.xbm)  "%(ICONPATH), end="");

	# Battery
	handle = open("/sys/bus/platform/devices/smapi/BAT0/installed", "r");
	installed = handle.read();
	handle.close();

	if (installed[0] == "1"): 

		handle = open("/sys/bus/platform/devices/smapi/BAT0/state", "r");
		state = handle.read();
		handle.close();

		handle = open("/sys/bus/platform/devices/smapi/BAT0/remaining_percent", "r");
		tpercent = handle.read();
		handle.close();

		percent = int(tpercent.strip());
		lpercent = percent/2;
		rpercent = (100-percent)/2

		print("^i(%s/bat_full_02.xbm) "%(ICONPATH), end="");

		if (state[:4] == "idle"): print("^fg(#FFE500)", end="");
		elif (state[:4] == "disc"): print("^fg(#FF4900)", end="");
		elif (state[:4] == "char"): print("^fg(#8EEB00)", end="");
		else: print("(%s)"%(state[:4]), end="");

		print("^r(%sx6)^fg(#909090)^r(%sx6)  "%(round(lpercent),round(rpercent)), end="");

	else:
		print("^i(%s/bat_empty_02.xbm)  "%(ICONPATH), end="");


	# CPUs

	for i in range(4):
		handle = open("/sys/devices/system/cpu/cpu%s/cpufreq/scaling_governor"%(i), "r");
		cpu = handle.read();
		handle.close();

		handle = open("/sys/devices/system/cpu/cpu%s/cpufreq/scaling_cur_freq"%(i), "r");
		tfreq = handle.read();
		handle.close();

		freq = int(tfreq.strip())/1000;
		freqcolor = {'pe': '#FF4900', 'po': '#8EEB00', 'co': '#FFE500', 'on': '#A600A6', 'us': '#0772A1'}

		print("^fg(%s)^i(%s/cpu.xbm) ^fg(#d0d0d0)%s  "%(freqcolor[cpu[:2]], ICONPATH, freq), end="");

	
	# Memory

	handle = open("/proc/meminfo", "r");
	meminfo = handle.read();
	handle.close();

	tmemtotal = re.search("MemTotal:\s+([0-9]+)", meminfo);
	tmemfree = re.search("MemFree:\s+([0-9]+)", meminfo);
	tcached = re.search("Cached:\s+([0-9]+)", meminfo);

	memtotal = int(tmemtotal.group(1));
	memfree = int(tmemfree.group(1));
	cached = int(tcached.group(1));
	memused = (memtotal-memfree)-cached;

	memtotal = memtotal/1024
	memfree = memfree/1024
	cached = cached/1024
	memused = memused/1024

	print("^fg(white)^i(%s/mem.xbm) ^fg(#FF4900)^r(%sx6)^fg(#FFE500)^r(%sx6)^fg(#8EEB00)^r(%sx6)^fg(white)"%(ICONPATH, round(memused/37.42), round(cached/37.42), round(memfree/37.42)), end="");

	# network

	networkinfo = subprocess.check_output(["/usr/sbin/ip", "addr"]).decode("utf-8");
	wlanup = re.search("wlan0:.*state (\S+)", networkinfo)
	if wlanup:
		wlanup = wlanup.group(1)
	else:
		wlanup = "DOWN"
	ethup = re.search("eth0:.*state (\S+)", networkinfo)
	if ethup:
		ethup = ethup.group(1)
	else:
		ethup = "DOWN"

	if (wlanup == "UP"):
		handle = open("/sys/class/net/wlan0/statistics/rx_bytes");
		wlanrx0 = int(handle.read());
		handle.close();
		handle = open("/sys/class/net/wlan0/statistics/tx_bytes");
		wlantx0 = int(handle.read());
		handle.close();

	if (ethup == "UP"):
		handle = open("/sys/class/net/eth0/statistics/rx_bytes");
		ethrx0 = int(handle.read());
		handle.close();
		handle = open("/sys/class/net/eth0/statistics/tx_bytes");
		ethtx0 = int(handle.read());
		handle.close();

	if (ethup == "UP" or wlanup == "UP"):
		ctime.sleep(1);

	if (wlanup == "UP"):
		handle = open("/sys/class/net/wlan0/statistics/rx_bytes");
		wlanrx1 = int(handle.read());
		handle.close();
		handle = open("/sys/class/net/wlan0/statistics/tx_bytes");
		wlantx1 = int(handle.read());
		handle.close();
		print("  ^fg(white)^i(%s/wifi_02.xbm) ^i(%s/net_down_03.xbm) ^fg(#d0d0d0)%s kB/s"%(ICONPATH, ICONPATH, str(round((wlanrx1-wlanrx0)/1024, 2)).rjust(5)), end="");
		print(" ^fg(white)^i(%s/net_up_03.xbm) ^fg(#d0d0d0)%s kB/s"%(ICONPATH, str(round((wlantx1-wlantx0)/1024, 2)).rjust(5)), end="");
	
	if (ethup == "UP"):
		handle = open("/sys/class/net/eth0/statistics/rx_bytes");
		ethrx1 = int(handle.read());
		handle.close();
		handle = open("/sys/class/net/eth0/statistics/tx_bytes");
		ethtx1 = int(handle.read());
		handle.close();
		print("  ^fg(white)^i(%s/net_wired.xbm) ^i(%s/net_down_03.xbm) ^fg(#d0d0d0)%s kB/s"%(ICONPATH, ICONPATH, str(round((ethrx1-ethrx0)/1024, 2)).rjust(5)), end="");
		print(" ^fg(white)^i(%s/net_up_03.xbm) ^fg(#d0d0d0)%s kB/s"%(ICONPATH, str(round((ethtx1-ethtx0)/1024, 2)).rjust(5)), end="");




	
	# Date/Time

	curdatetime = datetime.now();
	date = curdatetime.strftime("%y/%m/%d");
	tweekday = curdatetime.strftime("%w");
	weekday = int(tweekday)
	if (weekday == 0): weekday = 7;
	time = curdatetime.strftime("%H:%M");

	print("  ^fg(white)^i(%s/clock.xbm) ^fg(#d0d0d0)%s (%s) %s "%(ICONPATH, date,weekday,time), end="");

	# Mail
	newmail = len(os.listdir("/home/zhaozhou/.mail/inbox/new"))
	print("[ MAIL: %s ]"%(newmail), end="")


	# love
	thatdatetime = datetime(2011, 6, 10, 23, 30);
	delta = relativedelta(curdatetime, thatdatetime);
	print(" ^fg(red)^i(%s/heart.xbm) ^fg(white)0/%s/%s %s:%s "%(ICONPATH,delta.months,delta.days, delta.hours, delta.minutes));
