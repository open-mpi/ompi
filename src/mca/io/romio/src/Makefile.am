# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libmca_io_romio.la
libmca_io_romio_la_SOURCES = \
	io_romio.h           \
	io_romio_conv.h      \
	io_romio_global.c    \
	io_romio_module.c    \
	file_open.c          \
	file_read.c          \
	file_request.c       \
	file_write.c       

