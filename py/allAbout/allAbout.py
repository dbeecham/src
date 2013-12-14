#!/usr/bin/python

import types;

def allAbout(object):
	print object;
	print object.__doc__;
	print dir(object);
	print type(object);


if __name__ == "__main__":
	print allAbout(allAbout);
	print allAbout(1);
	print allAbout(1.1);
	print allAbout(types);
	print allAbout("");
	print allAbout(["hello", "hi"]);
	print allAbout(("hello", "hi"));
	print allAbout({"hello":"hi", "hi":"hello"});
