# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DLAM_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	ompi_os_path \
	lam_argv 

lam_argv_SOURCES = lam_argv.c
lam_argv_LDADD = \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/test/support/libsupport.la 
lam_argv_DEPENDENCIES = $(lam_argv_LDADD)

ompi_os_path_SOURCES = ompi_os_path.c
ompi_os_path_LDADD = \
	$(top_builddir)/src/util/sys_info.lo \
        $(top_builddir)/src/lfc/lam_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
	$(top_builddir)/src/util/os_path.lo \
        $(top_builddir)/test/support/libsupport.la 
ompi_os_path_DEPENDENCIES = $(lam_argv_LDADD)
