# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support

check_PROGRAMS = \
	ompi_bitmap \
	ompi_circular_buffer_fifo \
	ompi_fifo \
	ompi_hash_table \
	ompi_list \
	ompi_value_array \
	ompi_pointer_array \
	ompi_rb_tree \
	orte_bitmap

TESTS = \
	$(check_PROGRAMS)

ompi_bitmap_SOURCES = ompi_bitmap.c 
ompi_bitmap_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a
ompi_bitmap_DEPENDENCIES = $(ompi_bitmap_LDADD)

ompi_circular_buffer_fifo_SOURCES = ompi_circular_buffer_fifo.c 
ompi_circular_buffer_fifo_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a
ompi_circular_buffer_fifo_DEPENDENCIES = $(ompi_circular_buffer_fifo_LDADD)

ompi_fifo_SOURCES = ompi_fifo.c 
ompi_fifo_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a
ompi_fifo_DEPENDENCIES = $(ompi_fifo_LDADD)

ompi_list_SOURCES = ompi_list.c 
ompi_list_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a
ompi_list_DEPENDENCIES = $(ompi_list_LDADD)

ompi_hash_table_SOURCES = ompi_hash_table.c
ompi_hash_table_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a
ompi_hash_table_DEPENDENCIES = $(ompi_hash_table_LDADD)

ompi_pointer_array_SOURCES = ompi_pointer_array.c
ompi_pointer_array_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a
ompi_pointer_array_DEPENDENCIES = $(ompi_pointer_array_LDADD)

ompi_value_array_SOURCES = ompi_value_array.c
ompi_value_array_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a
ompi_value_array_DEPENDENCIES = $(ompi_value_array_LDADD)

ompi_rb_tree_SOURCES = ompi_rb_tree.c
ompi_rb_tree_LDADD = \
        $(top_builddir)/src/libmpi.la \
	$(top_builddir)/test/support/libsupport.a
ompi_rb_tree_DEPENDENCIES = $(ompi_rb_tree_LDADD)

orte_bitmap_SOURCES = orte_bitmap.c 
orte_bitmap_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a
orte_bitmap_DEPENDENCIES = $(orte_bitmap_LDADD)

