# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_builddir)/src/event

noinst_LTLIBRARIES = libmpiruntime.la

# Source code files

headers = \
	mpiruntime.h

libmpiruntime_la_SOURCES = \
	$(headers) \
        ompi_mpi_init.c \
        ompi_mpi_finalize.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mpiruntime
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
