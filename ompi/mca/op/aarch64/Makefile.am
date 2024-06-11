#
# Copyright (c) 2019      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This is an aarch64 op component.  This Makefile.am is a typical
# aarch64 of how to integrate into Open MPI's Automake-based build
# system.
#
# See https://github.com/open-mpi/ompi/wiki/devel-CreateComponent
# for more details on how to make Open MPI components.

# First, list all .h and .c sources.  It is necessary to list all .h
# files so that they will be picked up in the distribution tarball.

sources = \
    op_aarch64.h \
    op_aarch64_component.c

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
specialized_op_libs =
if MCA_BUILD_ompi_op_has_neon_support
specialized_op_libs += liblocal_ops_neon.la
liblocal_ops_neon_la_SOURCES = op_aarch64_functions.c
liblocal_ops_neon_la_CPPFLAGS = -DGENERATE_NEON_CODE
endif
if MCA_BUILD_ompi_op_has_sve_support
specialized_op_libs += liblocal_ops_sve.la
liblocal_ops_sve_la_SOURCES = op_aarch64_functions.c
liblocal_ops_sve_la_CPPFLAGS = -DGENERATE_SVE_CODE
endif

component_noinst  = $(specialized_op_libs)
if MCA_BUILD_ompi_op_aarch64_DSO
component_install = mca_op_aarch64.la
else
component_install =
component_noinst  += libmca_op_aarch64.la
endif

# Specific information for DSO builds.
#
# The DSO should install itself in $(ompilibdir) (by default,
# $prefix/lib/openmpi).

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_op_aarch64_la_SOURCES = $(sources)
mca_op_aarch64_la_LIBADD = $(specialized_op_libs)
mca_op_aarch64_la_LDFLAGS = -module -avoid-version $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la


# Specific information for static builds.
#
# Note that we *must* "noinst"; the upper-layer Makefile.am's will
# slurp in the resulting .la library into libmpi.

noinst_LTLIBRARIES = $(component_noinst)
libmca_op_aarch64_la_SOURCES = $(sources)
libmca_op_aarch64_la_LIBADD = $(specialized_op_libs)
libmca_op_aarch64_la_LDFLAGS = -module -avoid-version
