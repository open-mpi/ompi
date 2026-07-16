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
# Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS="-I$(top_srcdir)/test/support"

# Make "make check" work in an uninstalled --enable-mca-dso build tree
include $(top_srcdir)/Makefile.mca-dso-check

check_PROGRAMS = \
	opal_bitmap \
	opal_hash_table \
	opal_proc_table \
	opal_rb_tree \
	opal_list \
	opal_value_array \
	opal_pointer_array \
	opal_lifo \
	opal_fifo \
	opal_cstring \
	opal_interval_tree \
	opal_ring_buffer \
	opal_hotel \
	opal_graph

TESTS = $(check_PROGRAMS)

# Common link line for the OPAL unit tests
opal_test_ldadd = \
	$(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
	$(top_builddir)/test/support/libsupport.a

opal_bitmap_SOURCES = opal_bitmap.c
opal_bitmap_LDADD = $(opal_test_ldadd)
opal_bitmap_DEPENDENCIES = $(opal_test_ldadd)

opal_list_SOURCES = opal_list.c
opal_list_LDADD = $(opal_test_ldadd)
opal_list_DEPENDENCIES = $(opal_test_ldadd)

opal_hash_table_SOURCES = opal_hash_table.c
opal_hash_table_LDADD = $(opal_test_ldadd)
opal_hash_table_DEPENDENCIES = $(opal_test_ldadd)

opal_proc_table_SOURCES = opal_proc_table.c
opal_proc_table_LDADD = $(opal_test_ldadd)
opal_proc_table_DEPENDENCIES = $(opal_test_ldadd)

opal_pointer_array_SOURCES = opal_pointer_array.c
opal_pointer_array_LDADD = $(opal_test_ldadd)
opal_pointer_array_DEPENDENCIES = $(opal_test_ldadd)

opal_value_array_SOURCES = opal_value_array.c
opal_value_array_LDADD = $(opal_test_ldadd)
opal_value_array_DEPENDENCIES = $(opal_test_ldadd)

opal_rb_tree_SOURCES = opal_rb_tree.c
opal_rb_tree_LDADD = $(opal_test_ldadd)
opal_rb_tree_DEPENDENCIES = $(opal_test_ldadd)

opal_lifo_SOURCES = opal_lifo.c
opal_lifo_LDADD = $(opal_test_ldadd)
opal_lifo_DEPENDENCIES = $(opal_test_ldadd)

opal_fifo_SOURCES = opal_fifo.c
opal_fifo_LDADD = $(opal_test_ldadd)
opal_fifo_DEPENDENCIES = $(opal_test_ldadd)

opal_cstring_SOURCES = opal_cstring.c
opal_cstring_LDADD = $(opal_test_ldadd)
opal_cstring_DEPENDENCIES = $(opal_test_ldadd)

opal_interval_tree_SOURCES = opal_interval_tree.c
opal_interval_tree_LDADD = $(opal_test_ldadd)
opal_interval_tree_DEPENDENCIES = $(opal_test_ldadd)

opal_ring_buffer_SOURCES = opal_ring_buffer.c
opal_ring_buffer_LDADD = $(opal_test_ldadd)
opal_ring_buffer_DEPENDENCIES = $(opal_test_ldadd)

opal_hotel_SOURCES = opal_hotel.c
opal_hotel_LDADD = $(opal_test_ldadd)
opal_hotel_DEPENDENCIES = $(opal_test_ldadd)

opal_graph_SOURCES = opal_graph.c
opal_graph_LDADD = $(opal_test_ldadd)
opal_graph_DEPENDENCIES = $(opal_test_ldadd)

clean-local:
	rm -f opal_bitmap_test_out.txt opal_hash_table_test_out.txt opal_proc_table_test_out.txt

distclean-local:
	rm -rf *.dSYM .deps .libs *.log *.txt *.o *.trs $(check_PROGRAMS) Makefile
