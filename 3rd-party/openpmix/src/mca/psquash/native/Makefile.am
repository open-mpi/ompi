# -*- makefile -*-
#
# Copyright (c) 2019      IBM Corporation.  All rights reserved.
# Copyright (c) 2019      Mellanox Technologies, Inc.
#                         All rights reserved.
#
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers = psquash_native.h
sources = \
        psquash_native_component.c \
        psquash_native.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_psquash_native_DSO
lib =
lib_sources =
component = pmix_mca_psquash_native.la
component_sources = $(headers) $(sources)
else
lib = libpmix_mca_psquash_native.la
lib_sources = $(headers) $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component)
pmix_mca_psquash_native_la_SOURCES = $(component_sources)
pmix_mca_psquash_native_la_LDFLAGS = -module -avoid-version
if NEED_LIBPMIX
pmix_mca_psquash_native_la_LIBADD = $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(lib)
libpmix_mca_psquash_native_la_SOURCES = $(lib_sources)
libpmix_mca_psquash_native_la_LDFLAGS = -module -avoid-version
