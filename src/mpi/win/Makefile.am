# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmpi_win.la

# Source code files

headers = \
        win.h 

libmpi_win_la_SOURCES = \
        $(headers) \
	win.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/win
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
