# -*- makefile -*-
#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(prm_hpesmf_CPPFLAGS)

headers = prm_hpesmf.h
sources = \
        prm_hpesmf_component.c \
        prm_hpesmf.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_prm_hpesmf_DSO
lib =
lib_sources =
component = pmix_mca_prm_hpesmf.la
component_sources = $(headers) $(sources)
else
lib = libpmix_mca_prm_hpesmf.la
lib_sources = $(headers) $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component)
pmix_mca_prm_hpesmf_la_SOURCES = $(component_sources)
pmix_mca_prm_hpesmf_la_LIBADD = $(prm_hpesmf_LIBS)
pmix_mca_prm_hpesmf_la_LDFLAGS = -module -avoid-version $(prm_hpesmf_LDFLAGS)
if NEED_LIBPMIX
pmix_mca_prm_hpesmf_la_LIBADD += $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(lib)
libpmix_mca_prm_hpesmf_la_SOURCES = $(lib_sources)
libpmix_mca_prm_hpesmf_la_LDFLAGS = -module -avoid-version $(prm_hpesmf_LDFLAGS)
