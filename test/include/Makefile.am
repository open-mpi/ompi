# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
        atomic

atomic_SOURCES = atomic.c 
ompi_bitmap_LDADD = \
        $(top_builddir)/test/support/libsupport.la
ompi_bitmap_DEPENDENCIES = $(ompi_bitmap_LDADD)
