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
lamdir = $(includedir)/lam/request
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
