# -*- makefile -*-
#
# Copyright (c) 2026      Musawer Ahmad Saqif.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = -I$(top_srcdir)/test/support

# This test calls pthread_* APIs directly. The top-level thread tests use
# -lpthread for the same reason on all supported systems.
AM_LDFLAGS = -lpthread

# Make "make check" work in an uninstalled --enable-mca-dso build tree.
include $(top_srcdir)/Makefile.mca-dso-check

check_PROGRAMS = hw_resource_info
TESTS = $(check_PROGRAMS)

hw_resource_info_SOURCES = hw_resource_info.c
hw_resource_info_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(top_builddir)/test/support/libsupport.a
hw_resource_info_DEPENDENCIES = $(hw_resource_info_LDADD)
