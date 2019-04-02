#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2018 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from ompi/Makefile.am

dist_ompidata_DATA += runtime/help-mpi-runtime.txt

headers += \
        runtime/mpiruntime.h \
	runtime/ompi_cr.h \
        runtime/params.h \
	runtime/ompi_info_support.h \
	runtime/ompi_spc.h

lib@OMPI_LIBMPI_NAME@_la_SOURCES += \
        runtime/ompi_mpi_abort.c \
        runtime/ompi_mpi_dynamics.c \
        runtime/ompi_mpi_finalize.c \
        runtime/ompi_mpi_params.c \
        runtime/ompi_mpi_preconnect.c \
	runtime/ompi_cr.c \
	runtime/ompi_info_support.c \
	runtime/ompi_spc.c

# The MPIR portion of the library must be built with flags to
# enable stepping out of MPI_INIT into main.
# Use an intermediate library to isolate the debug object.
noinst_LTLIBRARIES += libompi_mpir.la
libompi_mpir_la_SOURCES = \
	runtime/ompi_mpi_init.c
libompi_mpir_la_CFLAGS = $(MPIR_UNWIND_CFLAGS)

lib@OMPI_LIBMPI_NAME@_la_LIBADD += libompi_mpir.la
