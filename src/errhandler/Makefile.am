# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = liberrhandler.la

# Source code files

headers = \
        errhandler.h \
        errhandler_predefined.h

liberrhandler_la_SOURCES = \
	$(headers) \
        errhandler.c \
        errhandler_invoke.c \
        errhandler_predefined.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/errhandler
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
