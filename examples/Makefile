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
# Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.  All rights reserved.
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
SHMEMCC = shmemcc
SHMEMFC = shmemfort

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
	 hello_oshmem \
	 hello_oshmemfh \
	 Hello.class \
	 ring_c \
	 ring_cxx \
	 ring_mpifh \
	 ring_usempi \
	 ring_usempif08 \
	 ring_oshmem \
	 ring_oshmemfh \
	 Ring.class \
	 connectivity_c \
	 oshmem_shmalloc \
	 oshmem_circular_shift \
	 oshmem_max_reduction \
	 oshmem_strided_puts \
	 oshmem_symmetric_data


# Default target.  Always build the C MPI examples.  Only build the
# others if we have the appropriate Open MPI / OpenSHMEM language
# bindings.

all: hello_c ring_c connectivity_c
	@ if which ompi_info >/dev/null 2>&1 ; then \
	    $(MAKE) mpi; \
	fi
	@ if which oshmem_info >/dev/null 2>&1 ; then \
	    $(MAKE) oshmem; \
	fi

# MPI examples

mpi:
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

# OpenSHMEM examples

oshmem:
	@ if oshmem_info --parsable | grep oshmem:bindings:c:yes >/dev/null; then \
	    $(MAKE) hello_oshmem; \
	    $(MAKE) ring_oshmem; \
	    $(MAKE) oshmem_shmalloc; \
	    $(MAKE) oshmem_circular_shift; \
	    $(MAKE) oshmem_max_reduction; \
	    $(MAKE) oshmem_strided_puts; \
	    $(MAKE) oshmem_symmetric_data; \
	fi
	@ if oshmem_info --parsable | grep oshmem:bindings:fort:yes >/dev/null; then \
	    $(MAKE) hello_oshmemfh; \
	    $(MAKE) ring_oshmemfh; \
	fi

# The usual "clean" target

clean:
	rm -f $(EXAMPLES) *~ *.o

# Don't rely on default rules for the Fortran and Java examples

hello_mpifh: hello_mpifh.f
	$(FC) $(FCFLAGS) $? -o $@
ring_mpifh: ring_mpifh.f
	$(FC) $(FCFLAGS) $? -o $@

hello_usempi: hello_usempi.f90
	$(FC) $(FCFLAGS) $? -o $@
ring_usempi: ring_usempi.f90
	$(FC) $(FCFLAGS) $? -o $@

hello_usempif08: hello_usempif08.f90
	$(FC) $(FCFLAGS) $? -o $@
ring_usempif08: ring_usempif08.f90
	$(FC) $(FCFLAGS) $? -o $@

Hello.class: Hello.java
	$(JAVAC) Hello.java
Ring.class: Ring.java
	$(JAVAC) Ring.java

hello_oshmem: hello_oshmem_c.c
	$(SHMEMCC) $(CFLAGS) $? -o $@
hello_oshmemfh: hello_oshmemfh.f90
	$(SHMEMFC) $(FCFLAGS) $? -o $@

ring_oshmem: ring_oshmem_c.c
	$(SHMEMCC) $(CFLAGS) $? -o $@
ring_oshmemfh: ring_oshmemfh.f90
	$(SHMEMFC) $(FCFLAGS) $? -o $@

oshmem_shmalloc: oshmem_shmalloc.c
	$(SHMEMCC) $(CCFLAGS) $? -o $@

oshmem_circular_shift: oshmem_circular_shift.c
	$(SHMEMCC) $(CFLAGS) $? -o $@

oshmem_max_reduction: oshmem_max_reduction.c
	$(SHMEMCC) $(CFLAGS) $? -o $@

oshmem_strided_puts: oshmem_strided_puts.c
	$(SHMEMCC) $(CFLAGS) $? -o $@

oshmem_symmetric_data: oshmem_symmetric_data.c
	$(SHMEMCC) $(CFLAGS) $? -o $@
