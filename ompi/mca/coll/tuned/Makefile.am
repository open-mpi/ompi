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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        coll_tuned.h \
        coll_tuned_topo.h \
        coll_tuned_util.h \
        coll_tuned_dynamic_file.h \
        coll_tuned_dynamic_rules.h \
        coll_tuned_forced.h \
        coll_tuned_topo.c \
        coll_tuned_util.c \
        coll_tuned_decision_fixed.c \
        coll_tuned_decision_dynamic.c \
        coll_tuned_dynamic_file.c \
        coll_tuned_dynamic_rules.c \
        coll_tuned_forced.c \
        coll_tuned_allreduce.c \
        coll_tuned_alltoall.c \
        coll_tuned_allgather.c \
        coll_tuned_barrier.c \
        coll_tuned_bcast.c \
        coll_tuned_reduce.c \
        coll_tuned_component.c \
        coll_tuned_module.c 

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_coll_tuned_DSO
component_noinst =
component_install = mca_coll_tuned.la
else
component_noinst = libmca_coll_tuned.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_tuned_la_SOURCES = $(sources)
mca_coll_tuned_la_LDFLAGS = -module -avoid-version
mca_coll_tuned_la_LIBADD = \
        $(top_ompi_builddir)/ompi/libmpi.la \
        $(top_ompi_builddir)/orte/libopen-rte.la \
        $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_tuned_la_SOURCES =$(sources)
libmca_coll_tuned_la_LDFLAGS = -module -avoid-version
