#
# Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017-2018 Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Need this so that the base knows where to load dynamic components from
# (by default)

AM_CPPFLAGS = \
    $(LTDLINCL)

noinst_LTLIBRARIES = libpmix_mca_base.la

dist_pmixdata_DATA = help-pmix-mca-base.txt help-pmix-mca-var.txt

# Source code files

headers = \
        base.h \
        pmix_mca_base_component_repository.h \
        pmix_mca_base_var.h \
        pmix_mca_base_var_enum.h \
        pmix_mca_base_var_group.h \
        pmix_mca_base_vari.h \
        pmix_mca_base_framework.h

# Library

libpmix_mca_base_la_SOURCES = \
        $(headers) \
        pmix_mca_base_close.c \
        pmix_mca_base_cmd_line.c \
        pmix_mca_base_component_compare.c \
        pmix_mca_base_component_find.c \
        pmix_mca_base_component_repository.c \
        pmix_mca_base_components_open.c \
        pmix_mca_base_components_close.c \
        pmix_mca_base_components_select.c \
        pmix_mca_base_list.c \
        pmix_mca_base_open.c \
        pmix_mca_base_var.c \
        pmix_mca_base_var_enum.c \
        pmix_mca_base_var_group.c \
        pmix_mca_base_parse_paramfile.c \
        pmix_mca_base_components_register.c \
        pmix_mca_base_framework.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
pmixdir = $(pmixincludedir)/$(subdir)
pmix_HEADERS = $(headers)
endif
