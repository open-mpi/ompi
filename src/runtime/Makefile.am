# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_builddir)/src/event

noinst_LTLIBRARIES = libruntime.la

# Source code files

headers = \
    runtime.h \
    runtime_types.h \
    runtime_internal.h \
    ompi_progress.h \
    orte_wait.h

libruntime_la_SOURCES = \
	$(headers) \
    orte_abort.c \
    ompi_progress.c \
    orte_finalize.c \
    orte_init.c \
    orte_monitor.c \
    orte_universe_exists.c \
    orte_restart.c \
    orte_schema.c \
    orte_wait.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/runtime
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
