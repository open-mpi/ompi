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
	scoll_basic.h \
	scoll_basic_module.c \
	scoll_basic_component.c \
	scoll_basic_barrier.c \
	scoll_basic_broadcast.c \
	scoll_basic_collect.c \
	scoll_basic_reduce.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_oshmem_scoll_basic_DSO
component_noinst =
component_install = mca_scoll_basic.la
else
component_noinst = libmca_scoll_basic.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_scoll_basic_la_SOURCES = $(sources)
mca_scoll_basic_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_scoll_basic_la_SOURCES =$(sources)
libmca_scoll_basic_la_LDFLAGS = -module -avoid-version
