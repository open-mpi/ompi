# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	ompi_numtostr \
	ompi_os_path \
	ompi_sys_info \
	ompi_os_create_dirpath \
	ompi_argv 

ompi_numtostr_SOURCES = ompi_numtostr.c
ompi_numtostr_LDADD = \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/src/util/numtostr.lo 
ompi_numtostr_DEPENDENCIES = $(ompi_numtostr_LDADD)

ompi_argv_SOURCES = ompi_argv.c
ompi_argv_LDADD = \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/test/support/libsupport.la 
ompi_argv_DEPENDENCIES = $(ompi_argv_LDADD)

ompi_os_path_SOURCES = ompi_os_path.c
ompi_os_path_LDADD = \
	$(top_builddir)/src/util/sys_info.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
	$(top_builddir)/src/util/os_path.lo \
        $(top_builddir)/test/support/libsupport.la 
ompi_os_path_DEPENDENCIES = $(ompi_os_path_LDADD)


ompi_sys_info_SOURCES = ompi_sys_info.c
ompi_sys_info_LDADD = \
	$(top_builddir)/src/util/sys_info.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/test/support/libsupport.la 
ompi_sys_info_DEPENDENCIES = $(ompi_sys_info_LDADD)

ompi_os_create_dirpath_SOURCES = ompi_os_create_dirpath.c
ompi_os_create_dirpath_LDADD = \
	$(top_builddir)/src/util/sys_info.lo \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/src/util/os_path.lo \
        $(top_builddir)/src/util/os_create_dirpath.lo \
        $(top_builddir)/test/support/libsupport.la 
ompi_os_create_dirpath_DEPENDENCIES = $(ompi_os_create_dirpath_LDADD)
