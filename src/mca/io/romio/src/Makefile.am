#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include \
        -I$(top_srcdir)

noinst_LTLIBRARIES = libmca_io_romio.la
libmca_io_romio_la_SOURCES = \
	io_romio.h \
	io_romio_component.c \
	io_romio_file_open.c \
	io_romio_file_read.c \
	io_romio_file_request.c \
	io_romio_file_write.c \
	io_romio_module.c

