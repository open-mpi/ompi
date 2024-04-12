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
# Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(psec_munge_CPPFLAGS)

headers = psec_munge.h
sources = \
        psec_munge_component.c \
        psec_munge.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_psec_munge_DSO
lib =
lib_sources =
component = pmix_mca_psec_munge.la
component_sources = $(headers) $(sources)
else
lib = libpmix_mca_psec_munge.la
lib_sources = $(headers) $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component)
pmix_mca_psec_munge_la_SOURCES = $(component_sources)
pmix_mca_psec_munge_la_LDFLAGS = -module -avoid-version $(psec_munge_LDFLAGS)
pmix_mca_psec_munge_la_LIBADD = $(psec_munge_LIBS)
if NEED_LIBPMIX
pmix_mca_psec_munge_la_LIBADD += $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(lib)
libpmix_mca_psec_munge_la_SOURCES = $(lib_sources)
libpmix_mca_psec_munge_la_LDFLAGS = -module -avoid-version $(psec_munge_LDFLAGS)
libpmix_mca_psec_munge_la_LIBADD = $(psec_munge_LIBS)
