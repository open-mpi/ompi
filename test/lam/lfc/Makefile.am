# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DLAM_ENABLE_DEBUG=1

noinst_PROGRAMS = list_test

list_test_SOURCES = list_test.c
list_test_LDADD = \
        $(top_builddir)/src/lam/lfc/list.lo \
        $(top_builddir)/src/lam/lfc/object.lo \
        $(top_builddir)/src/lam/mem/malloc.lo \
        $(top_builddir)/src/lam/util/output.lo \
        $(top_builddir)/src/lam/threads/mutex.lo \
        $(top_builddir)/test/support/libsupport.la
list_test_DEPENDENCIES = $(list_test_LDADD)

