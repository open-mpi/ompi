#!/usr/bin/env python3

from pmix import *
import signal, time
import os
import select
import subprocess
import argparse
import sys

global killer
class GracefulKiller:
  kill_now = False
  def __init__(self):
    signal.signal(signal.SIGINT, self.exit_gracefully)
    signal.signal(signal.SIGTERM, self.exit_gracefully)

  def exit_gracefully(self,signum, frame):
    self.kill_now = True

def main():
    test_count = 0
    test_fails = 0
    parser = argparse.ArgumentParser()
    parser.add_argument("-t", "--test", help="pass string name of test function (i.e test_alloc_info)")
    parser.add_argument("-a", "--all", action="store_true")
    args = parser.parse_args()
    unload = True
    if len(sys.argv) < 2:
        print("no arguments provided")
    if args.test == "test_alloc_info":
        test_alloc_info()
    elif args.test == "test_load_value":
        test_load_value(unload)
    elif args.all:
        test_load_value(unload)
        test_alloc_info()

if __name__ == '__main__':
    global killer
    killer = GracefulKiller()
    main()
