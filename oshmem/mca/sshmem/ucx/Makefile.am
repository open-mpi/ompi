# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#dist_oshmemdata_DATA = help-oshmem-sshmem-mmap.txt

AM_CPPFLAGS = $(sshmem_ucx_CPPFLAGS)

sources = \
        sshmem_ucx.h \
        sshmem_ucx_component.c \
        sshmem_ucx_module.c \
        sshmem_ucx_shadow.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_oshmem_sshmem_ucx_DSO
component_noinst =
component_install = mca_sshmem_ucx.la
else
component_noinst = libmca_sshmem_ucx.la
component_install =
endif

mcacomponentdir = $(oshmemlibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sshmem_ucx_la_SOURCES = $(sources)
mca_sshmem_ucx_la_LDFLAGS = -module -avoid-version $(sshmem_ucx_LDFLAGS)
mca_sshmem_ucx_la_LIBADD = $(top_builddir)/oshmem/liboshmem.la \
	$(sshmem_ucx_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_sshmem_ucx_la_SOURCES =$(sources)
libmca_sshmem_ucx_la_LDFLAGS = -module -avoid-version $(sshmem_ucx_LDFLAGS)

