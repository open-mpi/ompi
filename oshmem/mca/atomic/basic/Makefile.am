#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
	atomic_basic.h \
	atomic_basic_module.c \
	atomic_basic_component.c \
	atomic_basic_fadd.c \
	atomic_basic_cswap.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_oshmem_atomic_basic_DSO
component_noinst =
component_install = mca_atomic_basic.la
else
component_noinst = libmca_atomic_basic.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_atomic_basic_la_SOURCES = $(sources)
mca_atomic_basic_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_atomic_basic_la_SOURCES =$(sources)
libmca_atomic_basic_la_LDFLAGS = -module -avoid-version
