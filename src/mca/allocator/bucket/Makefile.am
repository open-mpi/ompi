# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libmca_allocator_bucket.la
libmca_allocator_bucket_la_SOURCES = \
	allocator_bucket.c \
	allocator_bucket_alloc.h \
        allocator_bucket_alloc.c
