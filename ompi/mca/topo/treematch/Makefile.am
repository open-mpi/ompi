#
# Copyright (c) 2011-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2011-2015 INRIA.  All rights reserved.
# Copyright (c) 2011-2015 Université Bordeaux 1
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2019      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2021      Google, LLC. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(topo_treematch_CPPFLAGS)

sources = \
    topo_treematch.h \
    topo_treematch_module.c \
    topo_treematch_component.c \
    topo_treematch_dist_graph_create.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_topo_treematch_DSO
lib =
lib_sources =
component = mca_topo_treematch.la
component_sources = $(sources)
else
lib = libmca_topo_treematch.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_topo_treematch_la_SOURCES = $(component_sources)
mca_topo_treematch_la_LDFLAGS = -module -avoid-version $(topo_treematch_LDFLAGS)
mca_topo_treematch_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la $(topo_treematch_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_topo_treematch_la_SOURCES = $(lib_sources)
libmca_topo_treematch_la_LIBADD = $(topo_treematch_LIBS)
libmca_topo_treematch_la_LDFLAGS = -module -avoid-version $(topo_treematch_LDFLAGS)

distclean-local:
	rm -f config.h
