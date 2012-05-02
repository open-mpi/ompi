#!/usr/bin/env python

from operator import itemgetter
import sys

words = []
cnt = []

# input comes from STDIN
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()

    # parse the input we got from mapper.py
    word, count = line.split('\t', 1)

    # convert count (currently a string) to int
    try:
        count = int(count)
    except ValueError:
        # count was not a number, so silently
        # ignore/discard this line
        continue

    # Add the word to the list count times
    try:
        n = words.index(word)
        cnt[n] += count
    except ValueError:
        words.append(word)
        cnt.append(count)

# output the final count
print 'Final word count:'
n=0
for wrd in words:
    print '%s\t%d' % (wrd, cnt[n])
    n += 1
