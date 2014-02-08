#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(atomic_mxm_CPPFLAGS)

mxm_sources = \
	atomic_mxm.h \
	atomic_mxm_module.c \
	atomic_mxm_component.c \
	atomic_mxm_fadd.c \
	atomic_mxm_cswap.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_oshmem_atomic_mxm_DSO
component_noinst =
component_install = mca_atomic_mxm.la
else
component_noinst = libmca_atomic_mxm.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_atomic_mxm_la_SOURCES = $(mxm_sources)
mca_atomic_mxm_la_LIBADD = $(atomic_mxm_LIBS)
mca_atomic_mxm_la_LDFLAGS = -module -avoid-version $(atomic_mxm_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_atomic_mxm_la_SOURCES =$(mxm_sources)
libmca_atomic_mxm_la_LDFLAGS = -module -avoid-version $(atomic_mxm_LDFLAGS)
