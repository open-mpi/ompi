#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This is an example op component.  This Makefile.am is a typical
# example of how to integrate into Open MPI's Automake-based build
# system.
#
# See https://svn.open-mpi.org/trac/ompi/wiki/devel/CreateComponent
# for more details on how to make Open MPI components.

# First, list all .h and .c sources.  It is necessary to list all .h
# files so that they will be picked up in the distribution tarball.

sources = \
    op_example.h \
    op_example_component.c \
    op_example_module_bxor.c \
    op_example_module_max.c

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

if MCA_BUILD_op_example_DSO
lib =
lib_sources =
component = mca_op_example.la
component_sources = $(sources)
else
lib = libmca_op_example.la
lib_sources = $(sources)
component =
component_sources =
endif

# Specific information for DSO builds.
#
# The DSO should install itself in $(pkglibdir) (by default,
# $prefix/lib/openmpi).

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_op_example_la_SOURCES = $(component_sources)
mca_op_example_la_LDFLAGS = -module -avoid-version

# Specific information for static builds.  
#
# Note that we *must* "noinst"; the upper-layer Makefile.am's will
# slurp in the resulting .la library into libmpi.

noinst_LTLIBRARIES = $(lib)
libmca_op_example_la_SOURCES = $(lib_sources)
libmca_op_example_la_LDFLAGS = -module -avoid-version
