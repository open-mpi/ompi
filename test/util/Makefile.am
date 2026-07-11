#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2011 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2018      Intel, Inc.  All rights reserved.
# Copyright (c) 2024      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = -I$(top_srcdir)/test/support

# Make "make check" work in an uninstalled --enable-mca-dso build tree
include $(top_srcdir)/Makefile.mca-dso-check

#check_PROGRAMS = \
#        opal_numtostr \
#        opal_error \
#        opal_if \
#        opal_os_path \
#        opal_timer \
#        opal_os_create_dirpath \
#        opal_argv \
#        opal_basename \
#        opal_path_nfs


check_PROGRAMS = \
	bipartite_graph \
	opal_bit_ops \
	opal_path_nfs \
        opal_json \
        opal_sha256 \
	opal_timer \
	opal_string_copy \
	opal_numtostr \
	opal_basename \
	opal_os_path \
	opal_getcwd \
	opal_argv \
	opal_environ \
	opal_printf \
	opal_uri \
	opal_qsort \
	opal_alfg \
	opal_crc \
	opal_fd \
	opal_arch \
	opal_cmd_line \
	opal_few \
	opal_info_subscriber \
	opal_keyval_parse \
	opal_malloc \
	opal_os_dirpath \
	opal_path \
	opal_proc \
	opal_sys_limits \
	opal_error \
	opal_if \
	opal_info \
	opal_net \
	opal_output

TESTS = \
	$(check_PROGRAMS)

# Common link line for the OPAL unit tests
opal_test_ldadd = \
	$(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
	$(top_builddir)/test/support/libsupport.a

opal_string_copy_SOURCES = opal_string_copy.c
opal_string_copy_LDADD = $(opal_test_ldadd)
opal_string_copy_DEPENDENCIES = $(opal_test_ldadd)

opal_numtostr_SOURCES = opal_numtostr.c
opal_numtostr_LDADD = $(opal_test_ldadd)
opal_numtostr_DEPENDENCIES = $(opal_test_ldadd)

opal_basename_SOURCES = opal_basename.c
opal_basename_LDADD = $(opal_test_ldadd)
opal_basename_DEPENDENCIES = $(opal_test_ldadd)

opal_os_path_SOURCES = opal_os_path.c
opal_os_path_LDADD = $(opal_test_ldadd)
opal_os_path_DEPENDENCIES = $(opal_test_ldadd)

opal_getcwd_SOURCES = opal_getcwd.c
opal_getcwd_LDADD = $(opal_test_ldadd)
opal_getcwd_DEPENDENCIES = $(opal_test_ldadd)

opal_argv_SOURCES = opal_argv.c
opal_argv_LDADD = $(opal_test_ldadd)
opal_argv_DEPENDENCIES = $(opal_test_ldadd)

opal_environ_SOURCES = opal_environ.c
opal_environ_LDADD = $(opal_test_ldadd)
opal_environ_DEPENDENCIES = $(opal_test_ldadd)

opal_printf_SOURCES = opal_printf.c
opal_printf_LDADD = $(opal_test_ldadd)
opal_printf_DEPENDENCIES = $(opal_test_ldadd)

opal_uri_SOURCES = opal_uri.c
opal_uri_LDADD = $(opal_test_ldadd)
opal_uri_DEPENDENCIES = $(opal_test_ldadd)

opal_qsort_SOURCES = opal_qsort.c
opal_qsort_LDADD = $(opal_test_ldadd)
opal_qsort_DEPENDENCIES = $(opal_test_ldadd)

opal_alfg_SOURCES = opal_alfg.c
opal_alfg_LDADD = $(opal_test_ldadd)
opal_alfg_DEPENDENCIES = $(opal_test_ldadd)

opal_crc_SOURCES = opal_crc.c
opal_crc_LDADD = $(opal_test_ldadd)
opal_crc_DEPENDENCIES = $(opal_test_ldadd)

opal_fd_SOURCES = opal_fd.c
opal_fd_LDADD = $(opal_test_ldadd)
opal_fd_DEPENDENCIES = $(opal_test_ldadd)

opal_arch_SOURCES = opal_arch.c
opal_arch_LDADD = $(opal_test_ldadd)
opal_arch_DEPENDENCIES = $(opal_test_ldadd)

opal_cmd_line_SOURCES = opal_cmd_line.c
opal_cmd_line_LDADD = $(opal_test_ldadd)
opal_cmd_line_DEPENDENCIES = $(opal_test_ldadd)

opal_few_SOURCES = opal_few.c
opal_few_LDADD = $(opal_test_ldadd)
opal_few_DEPENDENCIES = $(opal_test_ldadd)

opal_info_subscriber_SOURCES = opal_info_subscriber.c
opal_info_subscriber_LDADD = $(opal_test_ldadd)
opal_info_subscriber_DEPENDENCIES = $(opal_test_ldadd)

opal_keyval_parse_SOURCES = opal_keyval_parse.c
opal_keyval_parse_LDADD = $(opal_test_ldadd)
opal_keyval_parse_DEPENDENCIES = $(opal_test_ldadd)

opal_malloc_SOURCES = opal_malloc.c
opal_malloc_LDADD = $(opal_test_ldadd)
opal_malloc_DEPENDENCIES = $(opal_test_ldadd)

opal_os_dirpath_SOURCES = opal_os_dirpath.c
opal_os_dirpath_LDADD = $(opal_test_ldadd)
opal_os_dirpath_DEPENDENCIES = $(opal_test_ldadd)

opal_path_SOURCES = opal_path.c
opal_path_LDADD = $(opal_test_ldadd)
opal_path_DEPENDENCIES = $(opal_test_ldadd)

opal_proc_SOURCES = opal_proc.c
opal_proc_LDADD = $(opal_test_ldadd)
opal_proc_DEPENDENCIES = $(opal_test_ldadd)

opal_sys_limits_SOURCES = opal_sys_limits.c
opal_sys_limits_LDADD = $(opal_test_ldadd)
opal_sys_limits_DEPENDENCIES = $(opal_test_ldadd)

opal_error_SOURCES = opal_error.c
opal_error_LDADD = $(opal_test_ldadd)
opal_error_DEPENDENCIES = $(opal_test_ldadd)

opal_if_SOURCES = opal_if.c
opal_if_LDADD = $(opal_test_ldadd)
opal_if_DEPENDENCIES = $(opal_test_ldadd)

opal_info_SOURCES = opal_info.c
opal_info_LDADD = $(opal_test_ldadd)
opal_info_DEPENDENCIES = $(opal_test_ldadd)

opal_net_SOURCES = opal_net.c
opal_net_LDADD = $(opal_test_ldadd)
opal_net_DEPENDENCIES = $(opal_test_ldadd)

opal_output_SOURCES = opal_output.c
opal_output_LDADD = $(opal_test_ldadd)
opal_output_DEPENDENCIES = $(opal_test_ldadd)

#opal_numtostr_SOURCES = opal_numtostr.c
#opal_numtostr_LDADD = \
#        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
#        $(top_builddir)/test/support/libsupport.a
#opal_numtostr_DEPENDENCIES = $(opal_numtostr_LDADD)

#opal_error_SOURCES = opal_error.c
#opal_error_LDADD = \
#        $(top_builddir)/orte/libopen-rte.la \
#opal_error_DEPENDENCIES = $(opal_error_LDADD)

#opal_if_SOURCES = opal_if.c
#opal_if_LDADD = \
#        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
#        $(top_builddir)/test/support/libsupport.a
#opal_if_DEPENDENCIES = $(opal_if_LDADD)

#opal_argv_SOURCES = opal_argv.c
#opal_argv_LDADD = \
#        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
#        $(top_builddir)/test/support/libsupport.a
#opal_argv_DEPENDENCIES = $(opal_argv_LDADD)

#opal_basename_SOURCES = opal_basename.c
#opal_basename_LDADD = \
#        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
#        $(top_builddir)/test/support/libsupport.a
#opal_basename_DEPENDENCIES = $(opal_basename_LDADD)

opal_bit_ops_SOURCES = opal_bit_ops.c
opal_bit_ops_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(top_builddir)/test/support/libsupport.a
opal_bit_ops_DEPENDENCIES = $(opal_path_nfs_LDADD)

opal_path_nfs_SOURCES = opal_path_nfs.c
opal_path_nfs_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(top_builddir)/test/support/libsupport.a
opal_path_nfs_DEPENDENCIES = $(opal_path_nfs_LDADD)

#opal_os_path_SOURCES = opal_os_path.c
#opal_os_path_LDADD = \
#        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
#        $(top_builddir)/test/support/libsupport.a
#opal_os_path_DEPENDENCIES = $(opal_os_path_LDADD)

opal_timer_SOURCES = opal_timer.c
opal_timer_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(top_builddir)/test/support/libsupport.a
opal_timer_DEPENDENCIES = $(opal_timer_LDADD)


#orte_sys_info_SOURCES = orte_sys_info.c
#orte_sys_info_LDADD = \
#        $(top_builddir)/orte/libopen-rte.la \
#        $(top_builddir)/test/support/libsupport.a
#orte_sys_info_DEPENDENCIES = $(orte_sys_info_LDADD)

#opal_os_create_dirpath_SOURCES = opal_os_create_dirpath.c
#opal_os_create_dirpath_LDADD = \
#        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
#        $(top_builddir)/test/support/libsupport.a
#opal_os_create_dirpath_DEPENDENCIES = $(opal_os_create_dirpath_LDADD)

#orte_session_dir_SOURCES = orte_session_dir.c
#orte_session_dir_LDADD = \
#        $(top_builddir)/orte/libopen-rte.la \
#        $(top_builddir)/test/support/libsupport.a
#orte_session_dir_DEPENDENCIES = $(orte_session_dir_LDADD)

#orte_universe_setup_file_io_SOURCES = orte_universe_setup_file_io.c
#orte_universe_setup_file_io_LDADD = \
#        $(top_builddir)/orte/libopen-rte.la \
#        $(top_builddir)/test/support/libsupport.a
#orte_universe_setup_file_io_DEPENDENCIES = $(orte_universe_setup_file_io_LDADD)

bipartite_graph_SOURCES = bipartite_graph.c
bipartite_graph_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(top_builddir)/test/support/libsupport.a
bipartite_graph_DEPENDENCIES = $(bipartite_graph_LDADD)

opal_json_SOURCES = opal_json.c
opal_json_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(top_builddir)/test/support/libsupport.a
opal_json_DEPENDENCIES = $(opal_json_LDADD)

opal_sha256_SOURCES = opal_sha256.c
opal_sha256_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(top_builddir)/test/support/libsupport.a
opal_sha256_DEPENDENCIES = $(opal_sha256_LDADD)

clean-local:
	rm -f test_session_dir_out test-file opal_path_nfs.out

distclean-local:
	rm -rf *.dSYM .deps .libs *.out *.log *.o *.trs $(check_PROGRAMS) Makefile

