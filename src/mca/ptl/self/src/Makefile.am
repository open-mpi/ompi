# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_builddir)/src/ompi/event \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libmca_ptl_self.la
libmca_ptl_self_la_SOURCES = \
	ptl_self.c \
	ptl_self.h \
	ptl_self_module.c
