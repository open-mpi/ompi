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
# Copyright (c) 2006-2007 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
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
FC = mpifort
JAVAC = mpijavac

# Using -g is not necessary, but it is helpful for example programs,
# especially if users want to examine them with debuggers.  Note that
# gmake requires the CXXFLAGS macro, while other versions of make
# (such as Sun's make) require the CCFLAGS macro.

CFLAGS = -g
CXXFLAGS = -g
CCFLAGS = -g
FCFLAGS = -g

# Example programs to build

EXAMPLES = \
	 hello_c \
	 hello_cxx \
	 hello_mpifh \
	 hello_usempi \
	 hello_usempif08 \
	 Hello.class \
         ring_c \
	 ring_cxx \
	 ring_mpifh \
	 ring_usempi \
	 ring_usempif08 \
	 Ring.class \
	 connectivity_c

# Default target.  Always build the C example.  Only build the others
# if Open MPI was build with the relevant language bindings.

all: hello_c ring_c connectivity_c
	@ if ompi_info --parsable | grep bindings:cxx:yes >/dev/null; then \
	    $(MAKE) hello_cxx ring_cxx; \
	fi
	@ if ompi_info --parsable | grep bindings:mpif.h:yes >/dev/null; then \
	    $(MAKE) hello_mpifh ring_mpifh; \
	fi
	@ if ompi_info --parsable | grep bindings:use_mpi:yes >/dev/null; then \
	    $(MAKE) hello_usempi ring_usempi; \
	fi
	@ if ompi_info --parsable | grep bindings:use_mpi_f08:yes >/dev/null; then \
	    $(MAKE) hello_usempif08 ring_usempif08; \
	fi
	@ if ompi_info --parsable | grep bindings:java:yes >/dev/null; then \
	    $(MAKE) Hello.class Ring.class; \
	fi

# The usual "clean" target

clean:
	rm -f $(EXAMPLES) *~ *.o

# Don't rely on default rules for the Fortran and Java examples

hello_mpifh: hello_mpifh.f
	$(FC) $(FCFLAGS) $^ -o $@
ring_mpifh: ring_mpifh.f
	$(FC) $(FCFLAGS) $^ -o $@

hello_usempi: hello_usempi.f90
	$(FC) $(FCFLAGS) $^ -o $@
ring_usempi: ring_usempi.f90
	$(FC) $(FCFLAGS) $^ -o $@

hello_usempif08: hello_usempif08.f90
	$(FC) $(FCFLAGS) $^ -o $@
ring_usempif08: ring_usempif08.f90
	$(FC) $(FCFLAGS) $^ -o $@

Hello.class: Hello.java
	$(JAVAC) Hello.java
Ring.class: Ring.java
	$(JAVAC) Ring.java
