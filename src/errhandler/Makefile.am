# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = liberrhandler.la

# Source code files

headers = \
        errhandler.h

liberrhandler_la_SOURCES = \
	$(headers) \
        errhandler_invoke.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/errhandler
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
