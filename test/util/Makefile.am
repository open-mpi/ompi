#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1 -g

noinst_PROGRAMS = \
	ompi_numtostr \
	ompi_if \
	ompi_pack \
	ompi_os_path \
	ompi_sys_info \
	ompi_os_create_dirpath \
        ompi_session_dir \
	ompi_argv 

ompi_numtostr_SOURCES = ompi_numtostr.c
ompi_numtostr_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_numtostr_DEPENDENCIES = $(ompi_numtostr_LDADD)

ompi_if_SOURCES = ompi_if.c
ompi_if_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_if_DEPENDENCIES = $(ompi_if_LDADD)

ompi_argv_SOURCES = ompi_argv.c
ompi_argv_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_argv_DEPENDENCIES = $(ompi_argv_LDADD)

ompi_os_path_SOURCES = ompi_os_path.c
ompi_os_path_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_os_path_DEPENDENCIES = $(ompi_os_path_LDADD)


ompi_sys_info_SOURCES = ompi_sys_info.c
ompi_sys_info_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_sys_info_DEPENDENCIES = $(ompi_sys_info_LDADD)

ompi_os_create_dirpath_SOURCES = ompi_os_create_dirpath.c
ompi_os_create_dirpath_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_os_create_dirpath_DEPENDENCIES = $(ompi_os_create_dirpath_LDADD)

ompi_session_dir_SOURCES = ompi_session_dir.c
ompi_session_dir_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_session_dir_DEPENDENCIES = $(ompi_session_dir_LDADD)

ompi_pack_SOURCES = ompi_pack.c
ompi_pack_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
ompi_pack_DEPENDENCIES = $(ompi_pack_LDADD)
