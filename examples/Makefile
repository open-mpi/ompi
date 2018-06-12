#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2018 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2007 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2011-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.  All rights reserved.
# Copyright (c) 2017-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Use the Open MPI-provided wrapper compilers.

MPICC = mpicc
MPICXX = mpic++
MPIFC = mpifort
MPIJAVAC = mpijavac
SHMEMCC = shmemcc
SHMEMCXX = shmemc++
SHMEMFC = shmemfort

# Using -g is not necessary, but it is helpful for example programs,
# especially if users want to examine them with debuggers.  Note that
# gmake requires the CXXFLAGS macro, while other versions of make
# (such as Sun's make) require the CCFLAGS macro.

CFLAGS += -g
CXXFLAGS += -g
CCFLAGS += -g
FCFLAGS += -g

# Example programs to build

EXAMPLES = \
	 hello_c \
	 hello_cxx \
	 hello_mpifh \
	 hello_usempi \
	 hello_usempif08 \
	 hello_oshmem \
	 hello_oshmemcxx \
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
	 oshmem_symmetric_data \
	 spc_example


# Default target.  Always build the C MPI examples.  Only build the
# others if we have the appropriate Open MPI / OpenSHMEM language
# bindings.

all: hello_c ring_c connectivity_c spc_example
	@ if which ompi_info >/dev/null 2>&1 ; then \
	    $(MAKE) mpi; \
	fi
	@ if which oshmem_info >/dev/null 2>&1 ; then \
	    $(MAKE) oshmem; \
	fi

# MPI examples

mpi:
	@ if ompi_info --parsable | grep -q bindings:cxx:yes >/dev/null; then \
	    $(MAKE) hello_cxx ring_cxx; \
	fi
	@ if ompi_info --parsable | grep -q bindings:mpif.h:yes >/dev/null; then \
	    $(MAKE) hello_mpifh ring_mpifh; \
	fi
	@ if ompi_info --parsable | egrep -q bindings:use_mpi:\"\?yes >/dev/null; then \
	    $(MAKE) hello_usempi ring_usempi; \
	fi
	@ if ompi_info --parsable | grep -q bindings:use_mpi_f08:yes >/dev/null; then \
	    $(MAKE) hello_usempif08 ring_usempif08; \
	fi
	@ if ompi_info --parsable | grep -q bindings:java:yes >/dev/null; then \
	    $(MAKE) Hello.class Ring.class; \
	fi

# OpenSHMEM examples

oshmem:
	@ if oshmem_info --parsable | grep oshmem:bindings:c:yes >/dev/null; then \
	    $(MAKE) hello_oshmem; \
	    $(MAKE) hello_oshmemcxx; \
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

hello_c: hello_c.c
	$(MPICC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
ring_c: ring_c.c
	$(MPICC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
connectivity_c: connectivity_c.c
	$(MPICC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
spc_example: spc_example.c
	$(MPICC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

hello_cxx: hello_cxx.cc
	$(MPICXX) $(CXXFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
ring_cxx: ring_cxx.cc
	$(MPICXX) $(CXXFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

hello_mpifh: hello_mpifh.f
	$(MPIFC) $(FCFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
ring_mpifh: ring_mpifh.f
	$(MPIFC) $(FCFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

hello_usempi: hello_usempi.f90
	$(MPIFC) $(FCFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
ring_usempi: ring_usempi.f90
	$(MPIFC) $(FCFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

hello_usempif08: hello_usempif08.f90
	$(MPIFC) $(FCFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
ring_usempif08: ring_usempif08.f90
	$(MPIFC) $(FCFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

Hello.class: Hello.java
	$(MPIJAVAC) Hello.java
Ring.class: Ring.java
	$(MPIJAVAC) Ring.java

hello_oshmem: hello_oshmem_c.c
	$(SHMEMCC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
hello_oshmemcxx: hello_oshmem_cxx.cc
	$(SHMEMCXX) $(CXXFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
hello_oshmemfh: hello_oshmemfh.f90
	$(SHMEMFC) $(FCFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

ring_oshmem: ring_oshmem_c.c
	$(SHMEMCC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
ring_oshmemfh: ring_oshmemfh.f90
	$(SHMEMFC) $(FCFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

oshmem_shmalloc: oshmem_shmalloc.c
	$(SHMEMCC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

oshmem_circular_shift: oshmem_circular_shift.c
	$(SHMEMCC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

oshmem_max_reduction: oshmem_max_reduction.c
	$(SHMEMCC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

oshmem_strided_puts: oshmem_strided_puts.c
	$(SHMEMCC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@

oshmem_symmetric_data: oshmem_symmetric_data.c
	$(SHMEMCC) $(CFLAGS) $(LDFLAGS) $? $(LDLIBS) -o $@
