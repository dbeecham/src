#!/usr/bin/env python

""" Module for the class AC """

import pickle


class Ac:
    "The AC class figures out if the laptop is "\
    "plugged into AC power.\nIt'll report every "\
    "10 seconds."
    clock = 1

    def update(self, loop_iterator, iconpath):
        "update(iterator) -> none, \n"\
        "update() is the function which does the"\
        "bulk of the data processing.\n"\
        "It will read files and pickle other files."

        if (loop_iterator % self.clock == 0):
            handle = open("/sys/bus/platform/devices/smapi/ac_connected", "r")
            ac_connected = handle.read()
            handle.close()

            handle = open("/var/run/dzen_generator/ac", "wb")
            pickle.dump(ac_connected, handle)
            handle.close()
