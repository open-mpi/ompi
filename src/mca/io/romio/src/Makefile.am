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

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/include \
        -I$(top_ompi_builddir)/src \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include \
        -I$(top_srcdir) \
        -I$(top_srcdir)/romio-dist/include \
        -I$(top_builddir)

noinst_LTLIBRARIES = libmca_io_romio.la
libmca_io_romio_la_SOURCES = \
	io_romio.h \
	io_romio_component.c \
	io_romio_file_open.c \
	io_romio_file_read.c \
	io_romio_file_write.c \
	io_romio_module.c \
	io_romio_request.c

