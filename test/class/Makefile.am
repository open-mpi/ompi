# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	ompi_bitmap \
	ompi_circular_buffer_fifo \
	ompi_hash_table \
	ompi_list \
	ompi_value_array \
	ompi_pointer_array \
	ompi_rb_tree

ompi_bitmap_SOURCES = ompi_bitmap.c 
ompi_bitmap_LDADD = \
        $(top_builddir)/src/class/ompi_bitmap.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/test/support/libsupport.la
ompi_bitmap_DEPENDENCIES = $(ompi_bitmap_LDADD)

ompi_circular_buffer_fifo_SOURCES = ompi_circular_buffer_fifo.c 
ompi_circular_buffer_fifo_LDADD = \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/test/support/libsupport.la \
        $(top_builddir)/src/util/libutil.la
ompi_circular_buffer_fifo_DEPENDENCIES = $(ompi_circular_buffer_fifo_LDADD)

ompi_list_SOURCES = ompi_list.c 
ompi_list_LDADD = \
        $(top_builddir)/src/class/ompi_list.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/test/support/libsupport.la
ompi_list_DEPENDENCIES = $(ompi_list_LDADD)

ompi_hash_table_SOURCES = ompi_hash_table.c
ompi_hash_table_LDADD = \
        $(top_builddir)/src/class/ompi_hash_table.lo \
        $(top_builddir)/src/class/ompi_list.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/util/libutil.la \
        $(top_builddir)/test/support/libsupport.la

ompi_pointer_array_SOURCES = ompi_pointer_array.c
ompi_pointer_array_LDADD = \
        $(top_builddir)/src/class/ompi_pointer_array.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/util/libutil.la \
        $(top_builddir)/test/support/libsupport.la
ompi_pointer_array_DEPENDENCIES = $(ompi_pointer_array_LDADD)

ompi_value_array_SOURCES = ompi_value_array.c
ompi_value_array_LDADD = \
        $(top_builddir)/src/class/ompi_value_array.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/util/libutil.la \
        $(top_builddir)/test/support/libsupport.la
ompi_value_array_DEPENDENCIES = $(ompi_value_array_LDADD)

ompi_rb_tree_SOURCES = ompi_rb_tree.c
ompi_rb_tree_LDADD = \
	$(top_builddir)/test/support/libsupport.la \
	$(top_builddir)/src/class/ompi_free_list.lo \
	$(top_builddir)/src/class/ompi_rb_tree.lo \
	$(top_builddir)/src/class/ompi_object.lo \
	$(top_builddir)/src/class/ompi_list.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/malloc.lo 
ompi_rb_tree_DEPENDENCIES = $(ompi_rb_tree_LDADD)
