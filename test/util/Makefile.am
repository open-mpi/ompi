#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support

check_PROGRAMS = \
	ompi_numtostr \
	ompi_if \
	orte_os_path \
	orte_sys_info \
	orte_os_create_dirpath \
    orte_session_dir \
	ompi_argv \
    ompi_basename \
    orte_universe_setup_file_io

TESTS = \
	$(check_PROGRAMS)

ompi_numtostr_SOURCES = ompi_numtostr.c
ompi_numtostr_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a 
ompi_numtostr_DEPENDENCIES = $(ompi_numtostr_LDADD)

ompi_if_SOURCES = ompi_if.c
ompi_if_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a 
ompi_if_DEPENDENCIES = $(ompi_if_LDADD)

ompi_argv_SOURCES = ompi_argv.c
ompi_argv_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a 
ompi_argv_DEPENDENCIES = $(ompi_argv_LDADD)

ompi_basename_SOURCES = ompi_basename.c
ompi_basename_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a 
ompi_basename_DEPENDENCIES = $(ompi_basename_LDADD)

orte_os_path_SOURCES = orte_os_path.c
orte_os_path_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a 
orte_os_path_DEPENDENCIES = $(orte_os_path_LDADD)


orte_sys_info_SOURCES = orte_sys_info.c
orte_sys_info_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a 
orte_sys_info_DEPENDENCIES = $(orte_sys_info_LDADD)

orte_os_create_dirpath_SOURCES = orte_os_create_dirpath.c
orte_os_create_dirpath_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a 
orte_os_create_dirpath_DEPENDENCIES = $(orte_os_create_dirpath_LDADD)

orte_session_dir_SOURCES = orte_session_dir.c
orte_session_dir_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a 
orte_session_dir_DEPENDENCIES = $(orte_session_dir_LDADD)

orte_universe_setup_file_io_SOURCES = orte_universe_setup_file_io.c
orte_universe_setup_file_io_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.a 
orte_universe_setup_file_io_DEPENDENCIES = $(orte_universe_setup_file_io_LDADD)

