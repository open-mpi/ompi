# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmpi_communicator.la

# Source code files

headers = \
        communicator.h

libmpi_communicator_la_SOURCES = \
        $(headers) \
        mpi_comm_globals.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/communicator
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
