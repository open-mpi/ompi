# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DLAM_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	lam_argv 

lam_argv_SOURCES = lam_argv.c
lam_argv_LDADD = \
        $(top_builddir)/src/lam/lfc/lam_object.lo \
        $(top_builddir)/src/lam/mem/malloc.lo \
        $(top_builddir)/src/lam/util/output.lo \
        $(top_builddir)/src/lam/threads/mutex.lo \
        $(top_builddir)/src/lam/threads/mutex_pthread.lo \
        $(top_builddir)/src/lam/threads/mutex_spinlock.lo \
        $(top_builddir)/src/lam/threads/mutex_spinwait.lo \
        $(top_builddir)/test/support/libsupport.la 
lam_argv_DEPENDENCIES = $(lam_argv_LDADD)

