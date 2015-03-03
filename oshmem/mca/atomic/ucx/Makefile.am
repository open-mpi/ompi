#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(atomic_ucx_CPPFLAGS)

ucx_sources = \
	atomic_ucx.h \
	atomic_ucx_module.c \
	atomic_ucx_component.c \
	atomic_ucx_fadd.c \
	atomic_ucx_cswap.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_oshmem_atomic_ucx_DSO
component_noinst =
component_install = mca_atomic_ucx.la
else
component_noinst = libmca_atomic_ucx.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_atomic_ucx_la_SOURCES = $(ucx_sources)
mca_atomic_ucx_la_LIBADD = $(atomic_ucx_LIBS)
mca_atomic_ucx_la_LDFLAGS = -module -avoid-version $(atomic_ucx_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_atomic_ucx_la_SOURCES =$(ucx_sources)
libmca_atomic_ucx_la_LDFLAGS = -module -avoid-version $(atomic_ucx_LDFLAGS)

