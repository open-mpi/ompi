# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libop.la

# Source code files

headers = \
        op.h \
        op_predefined.h

libop_la_SOURCES = \
	$(headers) \
        op.c \
        op_predefined.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/op
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
