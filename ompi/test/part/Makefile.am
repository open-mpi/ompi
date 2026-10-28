# -*- makefile -*-
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

# These are single-process partitioned communication tests.  Each calls
# MPI_Init and performs all of its communication on MPI_COMM_SELF or with
# MPI_PROC_NULL, so they need no launcher and can run as part of 'make
# check'.  They exercise the MPI_Parrived null / inactive request behavior
# required by MPI-5.0 sec. 4.2.2 (Open MPI issue #14003), and partitioned
# communication with MPI_PROC_NULL (MPI-5.0 sec. 3.10).

# Make "make check" work in an uninstalled --enable-mca-dso build tree
include $(top_srcdir)/Makefile.mca-dso-check

check_PROGRAMS = \
        parrived_null_inactive \
        partitioned_proc_null
TESTS = $(check_PROGRAMS)

common_ldflags = $(OMPI_PKG_CONFIG_LDFLAGS)
# In ABI-only mode (--disable-ompi-abi) only libmpi_abi is built.  These
# tests use only the public MPI partitioned-communication API, so compile
# them with -DMPI_ABI (as the mpicc_abi wrapper does, making <mpi.h>
# resolve to the MPI Forum ABI header) and link libmpi_abi and libopen-pal
# only.  Otherwise link libmpi plus libopen_mpi (the latter is not always
# pulled in transitively by the uninstalled "make check" linker).
if OMPI_STANDARD_ABI_ONLY
# -DMPI_ABI makes <mpi.h> short-circuit to "standard_abi/mpi.h", which is
# generated in the build tree under ompi/mpi/c.
AM_CPPFLAGS = -DMPI_ABI -I$(top_builddir)/ompi/mpi/c
common_ldadd = \
        $(top_builddir)/ompi/lib@OMPI_MAKE_CHECK_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
else
common_ldadd = \
        $(top_builddir)/ompi/lib@OMPI_MAKE_CHECK_LIBMPI_NAME@.la \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
endif


parrived_null_inactive_SOURCES = parrived_null_inactive.c
parrived_null_inactive_LDFLAGS = $(common_ldflags)
parrived_null_inactive_LDADD = $(common_ldadd)

partitioned_proc_null_SOURCES = partitioned_proc_null.c
partitioned_proc_null_LDFLAGS = $(common_ldflags)
partitioned_proc_null_LDADD = $(common_ldadd)

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo *.log *.o *.trs \
		parrived_null_inactive partitioned_proc_null Makefile
