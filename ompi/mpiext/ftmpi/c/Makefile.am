#
# Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2016-2022 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2025-2026 Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers = \
    mpiext_ftmpi_c.h

# The bindings are compiled exactly once, with OMPI_BUILD_MPI_PROFILING=1:
# that compile emits the strong PMPIX_* entry points and the weak MPIX_*
# entry points (via a weak alias where the platform has them, and via a
# weak forwarding definition where it does not).
AM_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1

noinst_LTLIBRARIES = libmpiext_ftmpi_c.la

libmpiext_ftmpi_c_la_SOURCES = \
    comm_revoke.c \
    comm_is_revoked.c \
    comm_shrink.c \
    comm_ishrink.c \
    comm_failure_ack.c \
    comm_failure_get_acked.c \
    comm_get_failed.c \
    comm_ack_failed.c \
    comm_agree.c \
    comm_iagree.c

EXTRA_DIST = help-mpi-ft.txt

ompidir = $(ompiincludedir)/mpiext/
ompi_HEADERS = $(headers)
