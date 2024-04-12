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
# Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022-2023 Triad National Security, LLC. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(gds_shmem2_CPPFLAGS)

dist_pmixdata_DATA = help-gds-shmem2.txt

headers = \
        gds_shmem2.h \
        gds_shmem2_utils.h \
        gds_shmem2_store.h \
        gds_shmem2_fetch.h

sources = \
        gds_shmem2_component.c \
        gds_shmem2.c \
        gds_shmem2_utils.c \
        gds_shmem2_store.c \
        gds_shmem2_fetch.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_gds_shmem2_DSO
lib =
lib_sources =
component = pmix_mca_gds_shmem2.la
component_sources = $(headers) $(sources)
else
lib = libpmix_mca_gds_shmem2.la
lib_sources = $(headers) $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component)
pmix_mca_gds_shmem2_la_SOURCES = $(component_sources)
pmix_mca_gds_shmem2_la_LIBADD = $(gds_shmem2_LIBS)
pmix_mca_gds_shmem2_la_LDFLAGS = -module -avoid-version $(gds_shmem2_LDFLAGS)
if NEED_LIBPMIX
pmix_mca_gds_shmem2_la_LIBADD += $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(lib)
libpmix_mca_gds_shmem2_la_SOURCES = $(lib_sources)
libpmix_mca_gds_shmem2_la_LIBADD = $(gds_shmem2_LIBS)
libpmix_mca_gds_shmem2_la_LDFLAGS = -module -avoid-version $(gds_shmem2_LDFLAGS)
