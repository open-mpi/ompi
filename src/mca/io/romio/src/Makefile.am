# -*- makefile -*-
#
# $HEADER$
#

include $(top_lam_srcdir)/config/Makefile.options

SUBDIRS = romio-1.2.5.1

AM_CPPFLAGS = \
        -I$(top_lam_builddir)/src/include \
        -I$(top_lam_srcdir)/src \
        -I$(top_lam_srcdir)/src/include

noinst_LTLIBRARIES = libmca_io_romio.la
libmca_io_romio_la_SOURCES = \
	io_romio_module.c  \
	io_romio.h \
	file_request.c \
	file_open.c \
	file_read.c \
	file_write.c \
	global.c

