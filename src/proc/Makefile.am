# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libproc.la

# Source code files

headers = \
        proc.h

libproc_la_SOURCES = \
	$(headers) \
	proc.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/proc
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
