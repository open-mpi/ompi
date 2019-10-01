# -*- makefile -*-
#
# Copyright (c) 2017      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers = \
        pshmem_mmap.h

sources = \
        pshmem_mmap.c \
        pshmem_mmap_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_pshmem_mmap_DSO
lib =
lib_sources =
component = mca_pshmem_mmap.la
component_sources = $(headers) $(sources)
else
lib = libmca_pshmem_mmap.la
lib_sources = $(headers) $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_pshmem_mmap_la_SOURCES = $(component_sources)
mca_pshmem_mmap_la_LDFLAGS = -module -avoid-version
if NEED_LIBPMIX
mca_pshmem_mmap_la_LIBADD = $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(lib)
libmca_pshmem_mmap_la_SOURCES = $(lib_sources)
libmca_pshmem_mmap_la_LDFLAGS = -module -avoid-version
