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

SUBDIRS = profile

headers = \
    mpiext_ftmpi_c.h

# OMPI_BUILD_MPI_PROFILING is enabled when we want our generated MPI_* symbols
# to be replaced by PMPI_*.
# In this directory, we need it to be 0

AM_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=0

noinst_LTLIBRARIES = libmpiext_ftmpi_c.la

libmpiext_ftmpi_c_la_SOURCES = \
    comm_revoke.c \
    comm_is_revoked.c \
    comm_shrink.c \
    comm_failure_ack.c \
    comm_failure_get_acked.c \
    comm_get_failed.c \
    comm_ack_failed.c \
    comm_agree.c \
    comm_iagree.c
libmpiext_ftmpi_c_la_LIBADD = \
    profile/libpmpiext_ftmpi_c.la

dist_ompidata_DATA = help-mpi-ft.txt

ompidir = $(ompiincludedir)/mpiext/
ompi_HEADERS = $(headers)

MAINTAINERCLEANFILES = $(nodist_libmpiext_ftmpi_c_la_SOURCES)
