#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = 

buddy_sources = \
    memheap_buddy.c \
    memheap_buddy.h \
    memheap_buddy_component.c \
    memheap_buddy_component.h

#if OMPI_BUILD_memheap_buddy_DSO
if MCA_BUILD_ompi_pml_ob1_DSO
component_noinst =
component_install = mca_memheap_buddy.la
else
component_noinst = libmca_memheap_buddy.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_memheap_buddy_la_SOURCES = $(buddy_sources)
mca_memheap_buddy_la_LDFLAGS = -module -avoid-version

#noinst_LTLIBRARIES = $(lib)
noinst_LTLIBRARIES = $(component_noinst)
libmca_memheap_buddy_la_SOURCES = $(buddy_sources)
libmca_memheap_buddy_la_LDFLAGS = -module -avoid-version



