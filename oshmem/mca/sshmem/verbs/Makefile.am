# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(oshmem_verbs_CPPFLAGS)

sources = \
        sshmem_verbs.h \
        sshmem_verbs_component.c \
        sshmem_verbs_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_oshmem_sshmem_verbs_DSO
component_noinst =
component_install = mca_sshmem_verbs.la
else
component_noinst = libmca_sshmem_verbs.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sshmem_verbs_la_SOURCES = $(sources)
mca_sshmem_verbs_la_LDFLAGS = -module -avoid-version $(oshmem_verbs_LDFLAGS)
mca_sshmem_verbs_la_LIBADD  = $(oshmem_verbs_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_sshmem_verbs_la_SOURCES =$(sources)
libmca_sshmem_verbs_la_LDFLAGS = -module -avoid-version $(oshmem_verbs_LDFLAGS)
libmca_sshmem_verbs_la_LIBADD  = $(oshmem_verbs_LIBS)
