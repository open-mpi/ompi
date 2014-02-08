#
# Copyright (c) 2011-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2011-2013 INRIA.  All rights reserved.
# Copyright (c) 2011-2013 Université Bordeaux 1
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

sources = \
    topo_basic.h \
    topo_basic_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_topo_basic_DSO
lib =
lib_sources = 
component = mca_topo_basic.la
component_sources = $(sources)
else
lib = libmca_topo_basic.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_topo_basic_la_SOURCES = $(component_sources)
mca_topo_basic_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lib)
libmca_topo_basic_la_SOURCES = $(lib_sources)
libmca_topo_basic_la_LDFLAGS = -module -avoid-version

