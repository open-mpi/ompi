#
# Copyright (c) 2014      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
dist_ompidata_DATA = help-mpi-coll-cuda.txt

sources = coll_cuda_module.c coll_cuda_reduce.c coll_cuda_allreduce.c \
          coll_cuda_reduce_scatter_block.c coll_cuda_component.c \
          coll_cuda_scan.c coll_cuda_exscan.c coll_cuda.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_cuda_DSO
component_noinst =
component_install = mca_coll_cuda.la
else
component_noinst = libmca_coll_cuda.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_cuda_la_SOURCES = $(sources)
mca_coll_cuda_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_cuda_la_SOURCES =$(sources)
libmca_coll_cuda_la_LDFLAGS = -module -avoid-version

