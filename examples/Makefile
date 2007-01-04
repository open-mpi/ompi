#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the Open MPI-provided wrapper compilers.  Note that gmake
# requires the CXX macro, while other versions of make (such as Sun's
# make) require the CCC macro.

CC = mpicc
CXX = mpic++
CCC = mpic++
F77 = mpif77
FC = mpif90

# Using -g is not necessary, but it is helpful for example programs,
# especially if users want to examine them with debuggers.  Note that
# gmake requires the CXXFLAGS macro, while other versions of make
# (such as Sun's make) require the CCFLAGS macro.

CFLAGS = -g
CXXFLAGS = -g
CCFLAGS = -g
F77FLAGS = -g
FCFLAGS = -g

# Example programs to build

EXAMPLES = hello_c hello_cxx hello_f77 hello_f90 \
           ring_c ring_cxx ring_f77 ring_f90

# Default target.  Always build the C example.  Only build the others
# if Open MPI was build with the relevant language bindings.

all: hello_c ring_c
	@ if test "`ompi_info --parsable | grep bindings:cxx:yes`" != ""; then \
	    $(MAKE) hello_cxx ring_cxx; \
	fi
	@ if test "`ompi_info --parsable | grep bindings:f77:yes`" != ""; then \
	    $(MAKE) hello_f77 ring_f77; \
	fi
	@ if test "`ompi_info --parsable | grep bindings:f90:yes`" != ""; then \
	    $(MAKE) hello_f90 ring_f90; \
	fi


# The usual "clean" target

clean:
	rm -f $(EXAMPLES) *~ *.o

# Don't rely on default rules for the fortran examples

hello_f77: hello_f77.f
	$(F77) $(F77FLAGS) $^ -o $@
ring_f77: ring_f77.f
	$(F77) $(F77FLAGS) $^ -o $@

hello_f90: hello_f90.f90
	$(FC) $(FCFLAGS) $^ -o $@
ring_f90: ring_f90.f90
	$(FC) $(FCFLAGS) $^ -o $@

