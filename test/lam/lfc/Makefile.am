# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support

noinst_PROGRAMS = list_test

list_test_SOURCES = list_test.c
list_test_LDADD = \
        $(top_builddir)/test/support/libsupport.la
list_test_DEPENDENCIES = $(list_test_LDADD)

