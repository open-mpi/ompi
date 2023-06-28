#
# Copyright (c) 2023      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This component provides support for offloading reduce ops to CUDA devices.
#
# See https://github.com/open-mpi/ompi/wiki/devel-CreateComponent
# for more details on how to make Open MPI components.

# First, list all .h and .c sources.  It is necessary to list all .h
# files so that they will be picked up in the distribution tarball.

AM_CPPFLAGS = $(common_cuda_CPPFLAGS)

sources = op_cuda_component.c op_cuda.h op_cuda_functions.c op_cuda_impl.h
#sources_extended = op_cuda_functions.cu
cu_sources = op_cuda_impl.cu

NVCC = nvcc -O2

.cu.l$(OBJEXT):
	$(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=compile $(NVCC) -prefer-non-pic $(NVCCFLAGS) -Wc,-Xcompiler,-fPIC,-g -c $<

# -o $($@.o:.lo)

# Open MPI components can be compiled two ways:
#
# 1. As a standalone dynamic shared object (DSO), sometimes called a
# dynamically loadable library (DLL).
#
# 2. As a static library that is slurped up into the upper-level
# libmpi library (regardless of whether libmpi is a static or dynamic
# library).  This is called a "Libtool convenience library".
#
# The component needs to create an output library in this top-level
# component directory, and named either mca_<type>_<name>.la (for DSO
# builds) or libmca_<type>_<name>.la (for static builds).  The OMPI
# build system will have set the
# MCA_BUILD_ompi_<framework>_<component>_DSO AM_CONDITIONAL to indicate
# which way this component should be built.

if MCA_BUILD_ompi_op_cuda_DSO
component_install = mca_op_cuda.la
else
component_install =
component_noinst = libmca_op_cuda.la
endif

# Specific information for DSO builds.
#
# The DSO should install itself in $(ompilibdir) (by default,
# $prefix/lib/openmpi).

CUDADIR=/nfs/apps/spacks/2023-01-01/opt/spack/linux-centos7-x86_64/gcc-9.5.0/cuda-11.8.0-u2modnncfevx54ibr5dy27sxkirwsf7f

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_op_cuda_la_SOURCES = $(sources)
mca_op_cuda_la_LIBADD = $(cu_sources:.cu=.lo)
mca_op_cuda_la_LDFLAGS = -module -avoid-version $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
		$(accelerator_cuda_LIBS) -L$(CUDADIR)/lib64 -lcudart
EXTRA_mca_op_cuda_la_SOURCES = $(cu_sources)

# Specific information for static builds.
#
# Note that we *must* "noinst"; the upper-layer Makefile.am's will
# slurp in the resulting .la library into libmpi.

noinst_LTLIBRARIES = $(component_noinst)
libmca_op_cuda_la_SOURCES = $(sources)
libmca_op_cuda_la_LIBADD = $(cu_sources:.cu=.lo)
libmca_op_cuda_la_LDFLAGS = -module -avoid-version\
		$(accelerator_cuda_LIBS) -L$(CUDADIR)/lib64 -lcudart
EXTRA_libmca_op_cuda_la_SOURCES = $(cu_sources)

