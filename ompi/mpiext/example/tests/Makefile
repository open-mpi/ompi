#
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
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
        progress_c \
        progress_mpifh \
        progress_usempi \
        progress_usempif08

# Default target.  Always build the C example.  Only build the others
# if Open MPI was build with the relevant language bindings.

all: progress_c
	@ if ompi_info --parsable | grep bindings:mpif.h:yes >/dev/null; then \
	    $(MAKE) progress_mpifh; \
	fi
	@ if ompi_info --parsable | grep bindings:use_mpi:yes >/dev/null; then \
	    $(MAKE) progress_usempi; \
	fi
	@ if ompi_info --parsable | grep bindings:use_mpi_f08:yes >/dev/null; then \
	    $(MAKE) progress_usempif08; \
	fi

# The usual "clean" target

clean:
	rm -f $(EXAMPLES) *~ *.o

# Don't rely on default rules for the Fortran

progress_mpifh: progress_mpifh.F90
	$(FC) $(FCFLAGS) $^ -o $@
progress_usempi: progress_usempi.F90
	$(FC) $(FCFLAGS) $^ -o $@
progress_usempif08: progress_usempif08.F90
	$(FC) $(FCFLAGS) $^ -o $@
