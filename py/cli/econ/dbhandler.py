#!/usr/bin/env python3

import psycopg2

class DBConnectError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

class ConnectionHandle:

    handle = None
    cursor = None
    color = False


    colors = {'reset'   : '\033[1;m',
              'gray'    : '\033[1;30m',
              'red'     : '\033[1;31m',
              'green'   : '\033[1;32m',
              'yellow'  : '\033[1;33m',
              'blue'    : '\033[1;34m',
              'megenta' : '\033[1;35m',
              'cyan'    : '\033[1;36m',
              'white'   : '\033[1;37m',
              'crimson' : '\033[1;38m',
              'hred'    : '\033[1;41m',
              'hgreen'  : '\033[1;42m',
              'hyellow' : '\033[1;43m',
              'hblue'   : '\033[1;44m',
              'hmagenta': '\033[1;45m',
              'hcyan'   : '\033[1;46m',
              'hwhite'  : '\033[1;47m',
              'hcrimson': '\033[1;48m'
              }

    def enum(**enums):
        return type('Enum', (), enums)

    levels = enum(ERROR=1, 
                  WARNING=2, 
                  NORMAL=3, 
                  CUSTOM1=4, 
                  CUSTOM2=5, 
                  CUSTOM3=6, 
                  CUSTOM4=7,
                  CUSTOM5=8,
                  HILIGHT=9)

    def lout(self, string, level=3):
        if level == self.levels.NORMAL:
            clr1 = self.colors['yellow']
            clr2 = self.colors['green']
            splt = self.colors['gray']
            nrml = self.colors['reset']

        if level == self.levels.ERROR:
            clr1 = self.colors['red']
            clr2 = self.colors['yellow']
            splt = self.colors['gray']
            nrml = self.colors['red']

        string = "".join(string).split(":")

        if len(string) == 0:
            pass
        elif len(string) == 1:
            print("%s%s%s"%(nrml, string[0], self.colors['reset']))
        elif len(string) == 2:
            print("%s%s%s:%s%s%s"%(clr1, string[0], splt, nrml, string[1], self.colors['reset']))
        else:
            print("WHATATUHASUTNHAUSNTAHUAUTH")
            print(string)

    def lin(self, string=None, level=3, end='>'):
        if level == self.levels.NORMAL:
            clr1 = self.colors['yellow']
            clr2 = self.colors['green']
            splt = self.colors['gray']

        if type(string) == type(None):
            return input("%s %s %s"%(clr1, end.rjust(13), self.colors['reset']))
        else:
            return input("%s%s %s %s"%(string.rjust(12), clr1, end, self.colors['reset']))

    def commandline(self, cmd=None):
        while True:
            try:
                if cmd == None:
                    newcmd = self.lin()
                    newcmd = newcmd.split()
                    if len(newcmd) == 0:
                        continue
                else:
                    newcmd = self.lin("%s"%" ".join(cmd))
                    newcmd = newcmd.split()
                    if len(newcmd) == 0:
                        continue
                    newcmd = cmd + newcmd
            except (EOFError,KeyboardInterrupt):
                print()
                return

            try:
                func = getattr(self, newcmd[0])
            except (AttributeError,IndexError):
                if len(newcmd) == 0:
                    self.lout("Error: No command given.", self.levels.ERROR)
                    return
                else:
                    self.lout("Error: %s: no such command"%newcmd[0], self.levels.ERROR)
                    return

            func(newcmd)

    def listtable(self, table):
        self.lout("Listing everything in %s."%table)
        self.cursor.execute("select * from %s"%table)
        result = self.cursor.fetchone()

        if result == None:
            self.lout("There's nothing!")
        else:
            while result != None:
                self.lout("%s: %s"%(str(result[0]).rjust(3), result[1]))
                result = self.cursor.fetchone()

    def __init__(self, connectionString, options):
        if options&1 == 1:
            color = True

        try:
            self.handle = psycopg2.connect(connectionString)
            self.handle.autocommit = True
            self.cursor = self.handle.cursor()
        except psycopg2.OperationalError:
            raise DBConnectError("Could not connect to database.")

    def a(self, cmd):
        if len(cmd) == 1:
            self.commandline(cmd)
        else:
            if cmd[1] == "c":
                if len(cmd) == 3:
                    print("Adding category '%s'... "%cmd[2], end="")
                    self.cursor.execute("insert into category (name) values (%s)", (cmd[2],))
                    print("ok!")
                elif len(cmd) == 2:
                    print("You need to have a name for your category.")
                else:
                    print("Too many arguments.")
            elif cmd[1] == "u":
                if len(cmd) == 3:
                    print("Adding unit '%s'... "%cmd[2], end="")
                    self.cursor.execute("insert into unit (name) values (%s)", (cmd[2],))
                    print("ok!")
                elif len(cmd) == 2:
                    print("You need to have a name for your unit")
                else:
                    print("Too many arguments.")
            elif cmd[1] == "a":
                if len(cmd) == 3:
                    print("Adding brand '%s'... "%cmd[2], end="")
                    self.cursor.execute("insert into brand (name) values (%s)", (cmd[2],))
                    print("ok!")
                elif len(cmd) == 2:
                    print("You need to have a name for your brand")
                else:
                    print("Too many arguments.")
            elif cmd[1] == "b":
                if len(cmd) == 2:
                    # Default values
                    category = 1
                    date = "now"
                    brand = 1
                    quantity = 1
                    unit = 1

                    self.lout("\nGlobal settings following.")

                    newdate = self.lin("date", end=':')
                    if len(newdate) != 0:
                        self.cursor.execute("select date(%s)", (newdate,))
                        date = newdate

                    while True:
                        self.lout("\nLocal settings following.")

                        newcategory = self.lin("category", end=':')
                        if len(newcategory) != 0:
                            category = newcategory


                        newbrand = self.lin("brand", end=":")
                        if len(newbrand) != 0:
                            brand = newbrand

                        newunit = self.lin("unit", end=":")
                        if len(newunit) != 0:
                            unit = newunit

                        newquantity = self.lin("quantity", end=":")
                        if len(newquantity) != 0:
                            quantity = newquantity

                        title = self.lin("title", end=":")
                        if len(title) == 0:
                            self.lout("You need to enter a title.")
                            while title == "":
                                title = self.lin("title", end=":")

                        self.cursor.execute("insert into bought (title, quantity, date, unit, brand, category) values (%s, %s, %s, %s, %s, %s)", (title, quantity, date, unit, brand, category))
                        print("\n\n\tOK!\n\n")
            else:
                print("That's not a command.")

    def d(self, cmd):
        if len(cmd) == 1:
            self.commandline(cmd)
        else:
            if cmd[1] == "c":
                if len(cmd) == 3:
                    print("Deleting category '%s'... "%cmd[2], end="")
                    self.cursor.execute("delete from category where id = %s", (cmd[2],))
                    print("ok!")
                elif len(cmd) == 2:
                    print("You need to select which category to delete (by id).")
                else:
                    print("Too many arguments.")
            elif cmd[1] == "u":
                if len(cmd) == 3:
                    print("Deleting unit '%s'... "%cmd[2], end="")
                    self.cursor.execute("delete from unit where id = %s", (cmd[2],))
                    print("ok!")
                elif len(cmd) == 2:
                    print("You need to select which unit to delete (by id).")
                else:
                    print("Too many arguments.")
            elif cmd[1] == "a":
                if len(cmd) == 3:
                    print("Deleting brand '%s'... "%cmd[2], end="")
                    self.cursor.execute("delete from brand where id = %s", (cmd[2],))
                    print("ok!")
                elif len(cmd) == 2:
                    print("You need to select which brand to delete (by id).")
                else:
                    print("Too many arguments.")
            else:
                print("That's not a command!")

    def l(self, cmd):
        if len(cmd) == 1:
            self.commandline(cmd)
        else:
            if cmd[1] == 'c':
                if len(cmd) == 2:
                    self.listtable("category")

            elif cmd[1] == 'b':
                if len(cmd) == 2:
                    self.listtable("bought")

            elif cmd[1] == 'u':
                if len(cmd) == 2:
                    self.listtable("unit")

            elif cmd[1] == 'a':
                if len(cmd) == 2:
                    self.listtable("brand")

            else:
                print("usage: l [cbua] (c = category, b = bought, u = unit, a = brand")

    def question(self, cmd):
        print("No such command.")
