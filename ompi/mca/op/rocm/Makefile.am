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

# This component provides support for offloading reduce ops to ROCM devices.
#
# See https://github.com/open-mpi/ompi/wiki/devel-CreateComponent
# for more details on how to make Open MPI components.

# First, list all .h and .c sources.  It is necessary to list all .h
# files so that they will be picked up in the distribution tarball.

AM_CPPFLAGS = $(common_rocm_CPPFLAGS)

sources = op_rocm_component.c op_rocm.h op_rocm_functions.c op_rocm_impl.h
rocm_sources = op_rocm_impl.cpp

HIPCC = hipcc
#HIPCCFLAGS= -D__HIP_PLATFORM_HCC__= -D__HIP_PLATFORM_AMD__= -I/opt/rocm-5.5.0/include


.cpp.l$(OBJEXT):
	$(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=compile $(HIPCC) -O2 -fvectorize -prefer-non-pic $(HIPCCFLAGS) -Wc,-fPIC,-g -c $<

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

if MCA_BUILD_ompi_op_rocm_DSO
component_install = mca_op_rocm.la
else
component_install =
component_noinst = libmca_op_rocm.la
endif

# Specific information for DSO builds.
#
# The DSO should install itself in $(ompilibdir) (by default,
# $prefix/lib/openmpi).

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_op_rocm_la_SOURCES = $(sources)
mca_op_rocm_la_LIBADD = $(rocm_sources:.cpp=.lo)
mca_op_rocm_la_LDFLAGS = -module -avoid-version $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
		$(accelerator_rocm_LIBS) $(HIPCCFLAGS)
EXTRA_mca_op_rocm_la_SOURCES = $(rocm_sources)

# Specific information for static builds.
#
# Note that we *must* "noinst"; the upper-layer Makefile.am's will
# slurp in the resulting .la library into libmpi.

noinst_LTLIBRARIES = $(component_noinst)
libmca_op_rocm_la_SOURCES = $(sources)
libmca_op_rocm_la_LIBADD = $(rocm_sources:.cpp=.lo)
libmca_op_rocm_la_LDFLAGS = -module -avoid-version\
		$(accelerator_rocm_LIBS) ${HIPCCFLAGS}
EXTRA_libmca_op_rocm_la_SOURCES = $(rocm_sources)

