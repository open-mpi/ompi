# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-oshmem-sshmem-sysv.txt

sources = \
        sshmem_sysv.h \
        sshmem_sysv_component.c \
        sshmem_sysv_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_oshmem_sshmem_sysv_DSO
component_noinst =
component_install = mca_sshmem_sysv.la
else
component_noinst = libmca_sshmem_sysv.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sshmem_sysv_la_SOURCES = $(sources)
mca_sshmem_sysv_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sshmem_sysv_la_SOURCES = $(sources)
libmca_sshmem_sysv_la_LDFLAGS = -module -avoid-version
