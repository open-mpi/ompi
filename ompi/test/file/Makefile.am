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

# These are single-process OMPIO MPI_File info tests.  Each calls
# MPI_Init and performs all of its I/O on MPI_COMM_SELF, so they need no
# launcher and can run as part of 'make check'.  They exercise the OMPIO
# MPI_Info hint reporting required by MPI-5.0 sec. 15.2.8 (Open MPI issue
# #13367).

# Make "make check" work in an uninstalled --enable-mca-dso build tree
include $(top_srcdir)/Makefile.mca-dso-check

check_PROGRAMS = \
        file_info_defaults \
        file_info_hints \
        file_info_set_view \
        file_info_memkind
TESTS = $(check_PROGRAMS)

common_headers = ompio_file_info_common.h
common_ldflags = $(OMPI_PKG_CONFIG_LDFLAGS)
# These tests use MPI_* APIs (libmpi) but also reference internal OMPI
# symbols (e.g. predefined datatypes such as ompi_mpi_int, pulled in
# via MPI_* macro expansion) that live in libopen_mpi.  libopen_mpi is
# a dependency of libmpi, but the linker does not always pull it in
# transitively (e.g. in some uninstalled "make check" / distro linker
# configurations), so link it explicitly.
#
# In ABI-only mode (--disable-ompi-abi) only libmpi_abi is built: these
# tests use only the public MPI-IO API, so compile them with -DMPI_ABI
# (as the mpicc_abi wrapper does, making <mpi.h> resolve to the MPI Forum
# ABI header) and link libmpi_abi and libopen-pal only.
if OMPI_ABI_ONLY
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

file_info_defaults_SOURCES = file_info_defaults.c $(common_headers)
file_info_defaults_LDFLAGS = $(common_ldflags)
file_info_defaults_LDADD = $(common_ldadd)

file_info_hints_SOURCES = file_info_hints.c $(common_headers)
file_info_hints_LDFLAGS = $(common_ldflags)
file_info_hints_LDADD = $(common_ldadd)

file_info_set_view_SOURCES = file_info_set_view.c $(common_headers)
file_info_set_view_LDFLAGS = $(common_ldflags)
file_info_set_view_LDADD = $(common_ldadd)

file_info_memkind_SOURCES = file_info_memkind.c $(common_headers)
file_info_memkind_LDFLAGS = $(common_ldflags)
file_info_memkind_LDADD = $(common_ldadd)

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo *.log *.o *.trs \
		file_info_defaults file_info_hints file_info_set_view \
		file_info_memkind Makefile
