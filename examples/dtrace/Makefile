#
# Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
#			  Use is subject to license terms.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


BINS = mpicommleak

all: $(BINS)

mpicommleak: mpicommleak.c
	mpicc mpicommleak.c -o mpicommleak

clean:
	/bin/rm -f *.o

clobber:
	/bin/rm -f $(BINS) *.o
