# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	ompi_session_dir

ompi_session_dir_SOURCES = ompi_session_dir.c
ompi_session_dir_LDADD = \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/mem/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/src/util/os_path.lo \
        $(top_builddir)/src/util/os_create_dirpath.lo \
        $(top_builddir)/src/util/sys_info.lo \
        $(top_builddir)/src/rte/universe/os_session_dir.o \
        $(top_builddir)/test/support/libsupport.la 
ompi_session_dir_DEPENDENCIES = $(ompi_session_dir_LDADD)

