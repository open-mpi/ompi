#
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

# Use the Open MPI-provided wrapper compilers.

CC = mpicc
FC = mpifort

# Using -g is not necessary, but it is helpful for example programs,
# especially if users want to examine them with debuggers.

CFLAGS = -g
CCFLAGS = -g
FCFLAGS = -g

# Example programs to build

EXAMPLES = \
        errhandler_x \
        op_create_x

# Default target.  Always build the C example.  Only build the others
# if Open MPI was build with the relevant language bindings.

all: op_create_x

# The usual "clean" target

clean:
	rm -f $(EXAMPLES) *~ *.o

