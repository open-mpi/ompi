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

noinst_LTLIBRARIES = libmca_ptl_prof.la
libmca_ptl_prof_la_SOURCES = \
	ptl_prof.c \
	ptl_prof.h \
	ptl_prof_module.c
