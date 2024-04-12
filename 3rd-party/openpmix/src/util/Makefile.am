# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Source code files

AM_CPPFLAGS = \
    $(LTDLINCL)

noinst_LTLIBRARIES = libpmix_util.la

dist_pmixdata_DATA = \
    help-pmix-util.txt \
    help-cli.txt

headers = \
        pmix_alfg.h \
        pmix_argv.h \
        pmix_cmd_line.h \
        pmix_error.h \
        pmix_printf.h \
        pmix_output.h \
        pmix_environ.h \
        pmix_fd.h \
        pmix_timings.h \
        pmix_os_dirpath.h \
        pmix_os_path.h \
        pmix_basename.h \
        pmix_keyval_parse.h \
        pmix_show_help.h \
        pmix_path.h \
        pmix_getid.h \
        pmix_strnlen.h \
        pmix_shmem.h \
        pmix_vmem.h \
        pmix_hash.h \
        pmix_name_fns.h \
        pmix_net.h \
        pmix_if.h \
        pmix_parse_options.h \
        pmix_context_fns.h \
        pmix_pty.h \
        pmix_few.h \
        pmix_string_copy.h \
        pmix_getcwd.h

sources = \
        pmix_alfg.c \
        pmix_argv.c \
        pmix_cmd_line.c \
        pmix_error.c \
        pmix_printf.c \
        pmix_output.c \
        pmix_environ.c \
        pmix_fd.c \
        pmix_timings.c \
        pmix_os_dirpath.c \
        pmix_os_path.c \
        pmix_basename.c \
        pmix_keyval_parse.c \
        pmix_show_help.c \
        pmix_path.c \
        pmix_getid.c \
        pmix_shmem.c \
        pmix_vmem.c \
        pmix_hash.c \
        pmix_name_fns.c \
        pmix_net.c \
        pmix_if.c \
        pmix_parse_options.c \
        pmix_context_fns.c \
        pmix_pty.c \
        pmix_few.c \
        pmix_string_copy.c \
        pmix_getcwd.c

libpmix_util_la_SOURCES = $(headers) $(sources)

libpmix_util_la_LIBADD = \
        keyval/libpmixutilkeyval.la
libpmix_util_la_DEPENDENCIES = \
        keyval/libpmixutilkeyval.la

# Unconditionally install the header files

pmixdir = $(pmixincludedir)/$(subdir)
pmix_HEADERS = $(headers)
