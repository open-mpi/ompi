#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2014-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        prtedl_libltdl.h \
        prtedl_libltdl_component.c \
        prtedl_libltdl_module.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libprtemca_prtedl_libltdl.la

libprtemca_prtedl_libltdl_la_SOURCES = $(sources)
libprtemca_prtedl_libltdl_la_CPPFLAGS = $(prte_prtedl_libltdl_CPPFLAGS)
libprtemca_prtedl_libltdl_la_LDFLAGS = \
        $(prte_prtedl_libltdl_LDFLAGS) \
        -module -avoid-version
libprtemca_prtedl_libltdl_la_LIBADD = $(prte_prtedl_libltdl_LIBS)
