#!/usr/bin/env python
""" docstring """


from modules.settings import Settings
from modules.ac import Ac
from modules.battery import Battery
from time import sleep


if __name__ == "__main__":
    LOOP_ITERATOR = 0
    RESULT_STRING = ""
    ICONPATH = "/home/zhaozhou/.dzen2/icons/xbm8x8"

    AC = Ac()
    BATTERY = Battery()

    while True:
        # Run the modules.
        AC.update(LOOP_ITERATOR, ICONPATH)
        BATTERY.update(LOOP_ITERATOR, ICONPATH)

        LOOP_ITERATOR += 1
        sleep(0.3)
