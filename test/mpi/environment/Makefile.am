# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1
AM_LDFLAGS = -lpthread

noinst_PROGRAMS = chello

chello_SOURCES = chello.c
chello_LDADD = \
	$(top_builddir)/src/libmpi.la \
	$(top_builddir)/src/libompi.la 
chello_DEPENDENCIES = $(chello_LDADD)

