# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -g -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
    oob_test \
    oob_test_self \
	oob_test_packed

oob_test_SOURCES = oob_test.c
oob_test_LDADD = \
        $(top_builddir)/test/support/libsupport.la \
        $(top_builddir)/src/libmpi.la
oob_test_DEPENDENCIES = $(oob_test_LDADD)

oob_test_self_SOURCES = oob_test_self.c
oob_test_self_LDADD = \
        $(top_builddir)/test/support/libsupport.la \
        $(top_builddir)/src/libmpi.la
oob_test_self_DEPENDENCIES = $(oob_test_LDADD)

oob_test_packed_SOURCES = oob_test_packed.c
oob_test_packed_LDADD = \
        $(top_builddir)/test/support/libsupport.la \
        $(top_builddir)/src/libmpi.la
oob_test_packed_DEPENDENCIES = $(oob_test_LDADD)

