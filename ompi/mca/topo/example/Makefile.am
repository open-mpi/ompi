#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
    topo_example.h \
    topo_example_cart_map.c \
    topo_example_graph_map.c \
    topo_example_module.c \
    topo_example_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_topo_example_DSO
lib =
lib_sources =
component = mca_topo_example.la
component_sources = $(sources)
else
lib = libmca_topo_example.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_topo_example_la_SOURCES = $(component_sources)
mca_topo_example_la_LDFLAGS = -module -avoid-version
mca_topo_example_la_LIBADD = $(top_builddir)/ompi/libopen-mpi.la

noinst_LTLIBRARIES = $(lib)
libmca_topo_example_la_SOURCES = $(lib_sources)
libmca_topo_example_la_LDFLAGS = -module -avoid-version

