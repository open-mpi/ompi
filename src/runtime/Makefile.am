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
        universe_connect.h \
        universe_init.h \
	ompi_progress.h

libruntime_la_SOURCES = \
	$(headers) \
        ompi_abort.c \
        ompi_finalize.c \
        ompi_init.c \
        universe_connect.c \
        universe_init.c \
	ompi_progress.c \
        ompi_rte_finalize.c \
        ompi_rte_init.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/runtime
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
