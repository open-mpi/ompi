# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmpi_proc.la

# Source code files

headers = \
        proc.h

libmpi_proc_la_SOURCES = \
	$(headers) \
	proc.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/proc
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
