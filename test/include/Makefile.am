# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
        atomic

atomic_SOURCES = atomic.c 
atomic_LDADD = \
	$(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/class/ompi_object.lo \
	$(top_builddir)/src/util/malloc.lo \
	$(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/util/libutil.la \
        $(top_builddir)/test/support/libsupport.la
atomic_DEPENDENCIES = $(atomic_LDADD)
