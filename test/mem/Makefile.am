# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -I. -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	alloc_test 

alloc_test_SOURCES =  alloc_test.c 
alloc_test_LDADD = \
        $(top_builddir)/src/mca/mem/base/mem_base_allocator.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
	$(top_builddir)/src/class/ompi_object.lo \
	$(top_builddir)/src/mem/malloc.lo \
	$(top_builddir)/src/util/output.lo  

