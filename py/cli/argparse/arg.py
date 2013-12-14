#!/usr/bin/python

import argparse

parser = argparse.ArgumentParser(description="A Remember The Milk Command "
                                             "Line Interface",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter,
                                 prog="rtm-cli")

parser.add_argument("command", nargs="?")
parser.add_argument("arguments", nargs="*")

parser.add_argument("-c", "--comp", 
                    help="toggle display of completed tasks",
                    action="store_false")
parser.add_argument("-n", "--notes",
                    help="toggle the display of notes indicators",
                    action="store_false")
parser.add_argument("-p", "--plain",
                    help="set output to plain (no color)",
                    action="store_true")
parser.add_argument("-r", "--readline",
                    help="toggle readline support (disable to improve unicode"
                         "support in interactive mode)",
                    action="store_false")
parser.add_argument("-s", "--status",
                    help="toggle the display of status messages",
                    action="store_false")
parser.add_argument("-t", "--tags",
                    help="toggle the display of tags",
                    action="store_false")
parser.add_argument("-v", "--version",
                    help="display version information",
                    action="version",
                    version="%(prog)s 2.4.5")



args = parser.parse_args()

print(args)


