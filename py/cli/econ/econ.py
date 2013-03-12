#!/usr/bin/env python3

import psycopg2
import dbhandler

def main():

    try:
        connectionHandle = dbhandler.ConnectionHandle("dbname=econ user=postgres", 1)
    except dbhandler.DBConnectError:
        print("Could not not connect to database.")
        return

    connectionHandle.commandline()


if __name__ == "__main__": main()
