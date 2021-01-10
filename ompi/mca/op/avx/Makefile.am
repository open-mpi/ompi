#
# Copyright (c) 2019-2020 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2020-2021 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This component provide support for the Advanced Vector Extensions (AVX)
# available in recent versions of x86 processors.
#
# See https://github.com/open-mpi/ompi/wiki/devel-CreateComponent
# for more details on how to make Open MPI components.

# First, list all .h and .c sources.  It is necessary to list all .h
# files so that they will be picked up in the distribution tarball.

sources = op_avx_component.c op_avx.h
sources_extended = op_avx_functions.c

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

# We need to support all processors from early AVX to full AVX512 support, based on
# a decision made at runtime. So, we generate all combinations of capabilities, and
# we will select the most suitable (based on the processor flags) during the
# component initialization.
specialized_op_libs =
if MCA_BUILD_ompi_op_has_avx_support
specialized_op_libs += liblocal_ops_avx.la
liblocal_ops_avx_la_SOURCES = $(sources_extended)
liblocal_ops_avx_la_CFLAGS = @MCA_BUILD_OP_AVX_FLAGS@
liblocal_ops_avx_la_CPPFLAGS = -DGENERATE_AVX_CODE
if MCA_BUILD_ompi_op_has_sse3_support
liblocal_ops_avx_la_CPPFLAGS += -DGENERATE_SSE3_CODE
endif
if MCA_BUILD_ompi_op_has_sse41_support
liblocal_ops_avx_la_CPPFLAGS += -DGENERATE_SSE41_CODE
endif
endif
if MCA_BUILD_ompi_op_has_avx2_support
specialized_op_libs += liblocal_ops_avx2.la
liblocal_ops_avx2_la_SOURCES = $(sources_extended)
liblocal_ops_avx2_la_CFLAGS = @MCA_BUILD_OP_AVX2_FLAGS@
liblocal_ops_avx2_la_CPPFLAGS = -DGENERATE_SSE3_CODE -DGENERATE_SSE41_CODE -DGENERATE_AVX_CODE -DGENERATE_AVX2_CODE
endif
if MCA_BUILD_ompi_op_has_avx512_support
specialized_op_libs += liblocal_ops_avx512.la
liblocal_ops_avx512_la_SOURCES = $(sources_extended)
liblocal_ops_avx512_la_CFLAGS = @MCA_BUILD_OP_AVX512_FLAGS@
liblocal_ops_avx512_la_CPPFLAGS = -DGENERATE_SSE3_CODE -DGENERATE_SSE41_CODE -DGENERATE_AVX_CODE -DGENERATE_AVX2_CODE -DGENERATE_AVX512_CODE
endif

component_noinst = $(specialized_op_libs)
if MCA_BUILD_ompi_op_avx_DSO
component_install = mca_op_avx.la
else
component_install =
component_noinst += libmca_op_avx.la
endif

# Specific information for DSO builds.
#
# The DSO should install itself in $(ompilibdir) (by default,
# $prefix/lib/openmpi).

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_op_avx_la_SOURCES = $(sources)
mca_op_avx_la_LIBADD = $(specialized_op_libs)
mca_op_avx_la_LDFLAGS = -module -avoid-version $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la


# Specific information for static builds.
#
# Note that we *must* "noinst"; the upper-layer Makefile.am's will
# slurp in the resulting .la library into libmpi.

noinst_LTLIBRARIES = $(component_noinst)
libmca_op_avx_la_SOURCES = $(sources)
libmca_op_avx_la_LIBADD = $(specialized_op_libs)
libmca_op_avx_la_LDFLAGS = -module -avoid-version

