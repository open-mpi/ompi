# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = liberrhandler.la

# Source code files

headers = \
        errclass.h \
        errcode.h \
        errcode-internal.h \
        errhandler.h \
        errhandler_predefined.h

liberrhandler_la_SOURCES = \
	$(headers) \
        errhandler.c \
        errhandler_invoke.c \
        errhandler_predefined.c \
	errcode.c \
	errclass.c  \
	errcode-internal.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/errhandler
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
