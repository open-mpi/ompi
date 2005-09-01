#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_bml_base.la

# Source code files

headers = \
        base.h \
        bml_base_btl.h \
        bml_base_endpoint.h

libmca_bml_base_la_SOURCES = \
        $(headers) \
        bml_base_btl.c \
	bml_base_btl.h \
	bml_base_endpoint.h \
        bml_base_endpoint.c \
	bml_base_init.c \
	bml_base_close.c \
	bml_base_open.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mca/bml/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif


distclean-local:
	rm -f static-components.h
