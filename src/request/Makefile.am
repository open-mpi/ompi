# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = librequest.la

# Source code files

headers = \
        request.h

librequest_la_SOURCES = \
        $(headers) \
	request.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/request
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
