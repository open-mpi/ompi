# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libns.la

# Source code files

headers = \
    ns.h

libns_la_SOURCES = \
    $(headers) \
    replica/name_server.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/util
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
