# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS="-I$(top_srcdir)/test/support"

check_PROGRAMS = \
	ompi_bitmap \
	opal_hash_table \
	opal_list \
	opal_value_array \
	ompi_pointer_array \
	ompi_rb_tree

TESTS = \
	$(check_PROGRAMS)

ompi_bitmap_SOURCES = ompi_bitmap.c 
ompi_bitmap_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
ompi_bitmap_DEPENDENCIES = $(ompi_bitmap_LDADD)

#ompi_circular_buffer_fifo_SOURCES = ompi_circular_buffer_fifo.c 
#ompi_circular_buffer_fifo_LDADD = \
#        $(top_builddir)/ompi/libmpi.la \
#        $(top_builddir)/orte/libopen-rte.la \
#        $(top_builddir)/opal/libopen-pal.la \
#        $(top_builddir)/test/support/libsupport.a
#ompi_circular_buffer_fifo_DEPENDENCIES = $(ompi_circular_buffer_fifo_LDADD)

#ompi_fifo_SOURCES = ompi_fifo.c 
#ompi_fifo_LDADD = \
#        $(top_builddir)/ompi/libmpi.la \
#        $(top_builddir)/orte/libopen-rte.la \
#        $(top_builddir)/opal/libopen-pal.la \
#        $(top_builddir)/test/support/libsupport.a
#ompi_fifo_DEPENDENCIES = $(ompi_fifo_LDADD)

opal_list_SOURCES = opal_list.c 
opal_list_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_list_DEPENDENCIES = $(opal_list_LDADD)

opal_hash_table_SOURCES = opal_hash_table.c
opal_hash_table_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_hash_table_DEPENDENCIES = $(opal_hash_table_LDADD)

ompi_pointer_array_SOURCES = ompi_pointer_array.c
ompi_pointer_array_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
ompi_pointer_array_DEPENDENCIES = $(ompi_pointer_array_LDADD)

opal_value_array_SOURCES = opal_value_array.c
opal_value_array_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_value_array_DEPENDENCIES = $(opal_value_array_LDADD)

ompi_rb_tree_SOURCES = ompi_rb_tree.c
ompi_rb_tree_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la \
	$(top_builddir)/test/support/libsupport.a
ompi_rb_tree_DEPENDENCIES = $(ompi_rb_tree_LDADD)

clean-local:
	rm -f ompi_bitmap_test_out.txt opal_hash_table_test_out.txt
