# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS		= c cxx f77 f90

lib_LTLIBRARIES	= libmpi.la

libmpi_la_SOURCES	=
libmpi_la_LIBADD	= \
	c/libmpi_c.la \
	f77/libmpi_f77.la
