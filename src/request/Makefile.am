# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = librequest.la

# Source code files

headers = \
    grequest.h \
    request.h

librequest_la_SOURCES = \
    $(headers) \
    grequest.c \
	request.c \
    req_test.c \
    req_wait.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/request
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
