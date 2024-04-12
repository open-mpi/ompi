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
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(pstrg_lustre_CPPFLAGS)

headers = pstrg_lustre.h
sources = \
        pstrg_lustre_component.c \
        pstrg_lustre.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_pstrg_lustre_DSO
lib =
lib_sources =
component = pmix_mca_pstrg_lustre.la
component_sources = $(headers) $(sources)
else
lib = libpmix_mca_pstrg_lustre.la
lib_sources = $(headers) $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component)
pmix_mca_pstrg_lustre_la_SOURCES = $(component_sources)
pmix_mca_pstrg_lustre_la_LIBADD = $(pstrg_lustre_LIBS)
pmix_mca_pstrg_lustre_la_LDFLAGS = -module -avoid-version $(pstrg_lustre_LDFLAGS)
if NEED_LIBPMIX
pmix_mca_pstrg_lustre_la_LIBADD += $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(lib)
libpmix_mca_pstrg_lustre_la_SOURCES = $(lib_sources)
libpmix_mca_pstrg_lustre_la_LIBADD = $(pstrg_lustre_LIBS)
libpmix_mca_pstrg_lustre_la_LDFLAGS = -module -avoid-version $(pstrg_lustre_LDFLAGS)
