# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DLAM_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	lam_bitmap \
	lam_registry \
	lam_hash_table \
	lam_list \
	lam_value_array \
	lam_pointer_array

lam_bitmap_SOURCES = lam_bitmap.c 
lam_bitmap_LDADD = \
        $(top_builddir)/src/lfc/lam_bitmap.lo \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/test/support/libsupport.la
lam_bitmap_DEPENDENCIES = $(lam_bitmap_LDADD)

lam_list_SOURCES = lam_list.c 
lam_list_LDADD = \
        $(top_builddir)/src/lfc/lam_list.lo \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/test/support/libsupport.la
lam_list_DEPENDENCIES = $(lam_list_LDADD)

lam_registry_SOURCES = registry.c 
lam_registry_LDADD = \
        $(top_builddir)/src/lfc/lam_list.lo \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/test/support/libsupport.la
lam_registry_DEPENDENCIES = $(lam_list_LDADD)

lam_hash_table_SOURCES = lam_hash_table.c
lam_hash_table_LDADD = \
        $(top_builddir)/src/lfc/lam_hash_table.lo \
        $(top_builddir)/src/lfc/lam_list.lo \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/util/libutil.la \
        $(top_builddir)/test/support/libsupport.la

lam_pointer_array_SOURCES = lam_pointer_array.c
lam_pointer_array_LDADD = \
        $(top_builddir)/src/lfc/lam_pointer_array.lo \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/util/libutil.la \
        $(top_builddir)/test/support/libsupport.la
lam_pointer_array_DEPENDENCIES = $(lam_pointer_array_LDADD)

lam_value_array_SOURCES = lam_value_array.c
lam_value_array_LDADD = \
        $(top_builddir)/src/lfc/lam_value_array.lo \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/util/libutil.la \
        $(top_builddir)/test/support/libsupport.la
lam_value_array_DEPENDENCIES = $(lam_value_array_LDADD)


