# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libmca_mpool_sm.la
libmca_mpool_sm_la_SOURCES = \
	mpool_sm.c \
	mpool_sm.h \
	mpool_sm_mmap.c \
	mpool_sm_mmap.h \
	mpool_sm_component.c
