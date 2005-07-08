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

noinst_LTLIBRARIES = libmca_io_base.la

headers = \
        base.h \
        io_base_request.h

libmca_io_base_la_SOURCES = \
        $(headers) \
        io_base_component_list.c \
        io_base_close.c \
        io_base_delete.c \
        io_base_file_select.c \
        io_base_find_available.c \
        io_base_open.c \
        io_base_request.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mca/io/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif


distclean-local:
	rm -f static-components.h
