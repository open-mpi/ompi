# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_PROGRAMS = argv_test

argv_test_SOURCES = argv_test.c
argv_test_LDADD = \
        $(top_builddir)/src/lam/util/libutil.la
argv_test_DEPENDENCIES = $(argv_test_LDADD)
