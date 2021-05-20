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
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
# Copyright (c) 2021      Argonne National Laboratory.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(opal_qthreads_CPPFLAGS)

sources = \
    threads_qthreads.h \
    threads_qthreads_component.c \
    threads_qthreads_condition.c \
    threads_qthreads_module.c \
    threads_qthreads_mutex.h \
    threads_qthreads_threads.h \
    threads_qthreads_tsd.h

lib_sources = $(sources)

if MCA_BUILD_opal_threads_qthreads_DSO
component_noinst =
component_install = mca_threads_qthreads.la
else
component_noinst = libmca_threads_qthreads.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_threads_qthreads_la_SOURCES = $(sources)
mca_threads_qthreads_la_LDFLAGS = -module -avoid-version
mca_threads_qthreads_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(opal_qthreads_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_threads_qthreads_la_SOURCES = $(sources)
libmca_threads_qthreads_la_LDFLAGS = -module -avoid-version $(opal_qthreads_LDFLAGS)
libmca_threads_qthreads_la_LIBADD = $(opal_qthreads_LIBS)
