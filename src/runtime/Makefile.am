# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_builddir)/src/event

noinst_LTLIBRARIES = libruntime.la

# Source code files

headers = \
	runtime.h \
        universe_init.h \
        universe_connect.h \
	ompi_progress.h

libruntime_la_SOURCES = \
	$(headers) \
        ompi_abort.c \
        ompi_finalize.c \
        ompi_init.c \
        ompi_mpi_init.c \
        ompi_mpi_finalize.c \
	ompi_progress.c \
        universe_init.c \
        ompi_rte_finalize.c \
        universe_connect.c \
        ompi_rte_init.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/runtime
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
