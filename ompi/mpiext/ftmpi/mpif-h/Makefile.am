#
# Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2016-2022 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

AM_CPPFLAGS = -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS
headers = \
    mpiext_ftmpi_mpifh.h

noinst_HEADERS = prototypes_mpi.h

f77_sources = \
    comm_revoke_f.c \
    comm_is_revoked_f.c \
    comm_shrink_f.c \
    comm_get_failed_f.c \
    comm_ack_failed_f.c \
    comm_failure_ack_f.c \
    comm_failure_get_acked_f.c \
    comm_agree_f.c \
    comm_iagree_f.c

f77_lib = libmpiext_ftmpi_mpifh.la
f77_lib_sources = $(f77_sources)

libmpiext_ftmpi_mpifh_la_SOURCES = $(f77_lib_sources) $(headers)
libmpiext_ftmpi_mpifh_la_LIBADD  = $(lib_sources)
libmpiext_ftmpi_mpifh_la_LDFLAGS = -module -avoid-version

MAINTAINERCLEANFILES = $(nodist_libmpiext_ftmpi_mpifh_la_SOURCES)
endif

noinst_LTLIBRARIES = $(f77_lib)

ompidir = $(ompiincludedir)/mpiext/
ompi_HEADERS = $(headers)
