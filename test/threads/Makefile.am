# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1
AM_LDFLAGS = -lpthread

noinst_PROGRAMS = \
	ompi_thread \
	ompi_condition

ompi_thread_SOURCES = ompi_thread.c
ompi_thread_LDADD = \
        $(top_builddir)/src/threads/thread.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/runtime/ompi_progress.lo \
        $(top_builddir)/src/event/libevent.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_thread_DEPENDENCIES = $(ompi_thread_LDADD)

ompi_condition_SOURCES = ompi_condition.c
ompi_condition_LDADD = \
        $(top_builddir)/src/threads/condition_pthread.lo \
        $(top_builddir)/src/threads/condition_spinwait.lo \
        $(top_builddir)/src/threads/condition_spinlock.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/thread.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/runtime/ompi_progress.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/event/libevent.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_condition_DEPENDENCIES = $(ompi_condition_LDADD)

