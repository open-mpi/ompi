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

ptmalloc_sources = \
				   malloc.c \
				   memheap_ptmalloc.c \
				   memheap_ptmalloc.h \
				   memheap_ptmalloc_component.c \
				   memheap_ptmalloc_component.h \
				   malloc_defs.h

#if MCA_BUILD_ompi_memheap_ptmalloc_DSO
if MCA_BUILD_ompi_pml_ob1_DSO
component_noinst =
component_install = mca_memheap_ptmalloc.la
else
component_noinst = libmca_memheap_ptmalloc.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_memheap_ptmalloc_la_SOURCES = $(ptmalloc_sources)
mca_memheap_ptmalloc_la_LDFLAGS = -module -avoid-version

#noinst_LTLIBRARIES = $(lib)
noinst_LTLIBRARIES = $(component_noinst)
libmca_memheap_ptmalloc_la_SOURCES = $(ptmalloc_sources)
libmca_memheap_ptmalloc_la_LDFLAGS = -module -avoid-version



