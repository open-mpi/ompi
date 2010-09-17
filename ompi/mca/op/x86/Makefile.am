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

sources = \
    op_x86.h \
    op_x86_component.c \
    op_x86_module_sum.c

if MCA_BUILD_ompi_op_x86_DSO
lib =
lib_sources =
component = mca_op_x86.la
component_sources = $(sources)
else
lib = libmca_op_x86.la
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
mca_op_x86_la_SOURCES = $(component_sources)
mca_op_x86_la_LDFLAGS = -module -avoid-version

# Specific information for static builds.  
#
# Note that we *must* "noinst"; the upper-layer Makefile.am's will
# slurp in the resulting .la library into libmpi.

noinst_LTLIBRARIES = $(lib)
libmca_op_x86_la_SOURCES = $(lib_sources)
libmca_op_x86_la_LDFLAGS = -module -avoid-version
