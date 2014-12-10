# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2007 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS="-I$(top_srcdir)/test/support"

if PROJECT_OMPI
  REQUIRES_OMPI = ompi_rb_tree
endif

check_PROGRAMS = \
	$(REQUIRES_OMPI) opal_bitmap \
	opal_hash_table \
	opal_proc_table \
	opal_tree \
	opal_list \
	opal_value_array \
	opal_pointer_array \
	opal_lifo \
	opal_fifo

TESTS = $(check_PROGRAMS)

opal_bitmap_SOURCES = opal_bitmap.c
opal_bitmap_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_bitmap_DEPENDENCIES = $(opal_bitmap_LDADD)

opal_list_SOURCES = opal_list.c
opal_list_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_list_DEPENDENCIES = $(opal_list_LDADD)

opal_tree_SOURCES = opal_tree.c
opal_tree_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_tree_DEPENDENCIES = $(opal_tree_LDADD)

opal_hash_table_SOURCES = opal_hash_table.c
opal_hash_table_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_hash_table_DEPENDENCIES = $(opal_hash_table_LDADD)

opal_proc_table_SOURCES = opal_proc_table.c
opal_proc_table_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_proc_table_DEPENDENCIES = $(opal_proc_table_LDADD)

opal_pointer_array_SOURCES = opal_pointer_array.c
opal_pointer_array_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_pointer_array_DEPENDENCIES = $(opal_pointer_array_LDADD)

opal_value_array_SOURCES = opal_value_array.c
opal_value_array_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_value_array_DEPENDENCIES = $(opal_value_array_LDADD)

ompi_rb_tree_SOURCES = ompi_rb_tree.c
ompi_rb_tree_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
	$(top_builddir)/test/support/libsupport.a
ompi_rb_tree_DEPENDENCIES = $(ompi_rb_tree_LDADD)

opal_lifo_SOURCES = opal_lifo.c
opal_lifo_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
	$(top_builddir)/test/support/libsupport.a
opal_lifo_DEPENDENCIES = $(opal_lifo_LDADD)

opal_fifo_SOURCES = opal_fifo.c
opal_fifo_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
	$(top_builddir)/test/support/libsupport.a
opal_fifo_DEPENDENCIES = $(opal_fifo_LDADD)

clean-local:
	rm -f opal_bitmap_test_out.txt opal_hash_table_test_out.txt opal_proc_table_test_out.txt
