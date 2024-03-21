#
# Copyright (c) 2014      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2014-2024 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = coll_accelerator_module.c coll_accelerator_reduce.c coll_accelerator_allreduce.c \
          coll_accelerator_reduce_scatter_block.c coll_accelerator_component.c \
          coll_accelerator_scan.c coll_accelerator_exscan.c coll_accelerator.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_accelerator_DSO
component_noinst =
component_install = mca_coll_accelerator.la
else
component_noinst = libmca_coll_accelerator.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_accelerator_la_SOURCES = $(sources)
mca_coll_accelerator_la_LDFLAGS = -module -avoid-version
mca_coll_accelerator_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_accelerator_la_SOURCES =$(sources)
libmca_coll_accelerator_la_LDFLAGS = -module -avoid-version

