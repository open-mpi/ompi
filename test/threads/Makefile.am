# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DLAM_ENABLE_DEBUG_OVERRIDE=1
AM_LDFLAGS = -lpthread

noinst_PROGRAMS = \
	lam_thread \
	lam_condition

lam_thread_SOURCES = lam_thread.c
lam_thread_LDADD = \
        $(top_builddir)/src/threads/thread.lo \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/runtime/lam_progress.lo \
        $(top_builddir)/src/event/libevent.la \
        $(top_builddir)/test/support/libsupport.la 
lam_thread_DEPENDENCIES = $(lam_thread_LDADD)

lam_condition_SOURCES = lam_condition.c
lam_condition_LDADD = \
        $(top_builddir)/src/threads/condition_pthread.lo \
        $(top_builddir)/src/threads/condition_spinwait.lo \
        $(top_builddir)/src/threads/condition_spinlock.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/thread.lo \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/runtime/lam_progress.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/event/libevent.la \
        $(top_builddir)/test/support/libsupport.la 
lam_condition_DEPENDENCIES = $(lam_condition_LDADD)

