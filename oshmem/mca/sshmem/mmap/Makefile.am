# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-oshmem-sshmem-mmap.txt

sources = \
        sshmem_mmap.h \
        sshmem_mmap_component.c \
        sshmem_mmap_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_oshmem_sshmem_mmap_DSO
component_noinst =
component_install = mca_sshmem_mmap.la
else
component_noinst = libmca_sshmem_mmap.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sshmem_mmap_la_SOURCES = $(sources)
mca_sshmem_mmap_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sshmem_mmap_la_SOURCES =$(sources)
libmca_sshmem_mmap_la_LDFLAGS = -module -avoid-version
