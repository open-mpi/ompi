# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libinfo.la

# Source code files

headers = \
        info.h

libinfo_la_SOURCES = \
        $(headers) \
        info.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/info
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
