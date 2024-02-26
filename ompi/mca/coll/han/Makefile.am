#
# Copyright (c) 2018-2020 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# Copyright (c) 2022      BULL S.A.S. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
coll_han.h \
coll_han_trigger.h \
coll_han_algorithms.h \
coll_han_dynamic.h \
coll_han_dynamic_file.h \
coll_han_barrier.c \
coll_han_bcast.c \
coll_han_reduce.c \
coll_han_scatter.c \
coll_han_gather.c \
coll_han_gatherv.c \
coll_han_allreduce.c \
coll_han_allgather.c \
coll_han_component.c \
coll_han_module.c \
coll_han_trigger.c \
coll_han_algorithms.c \
coll_han_dynamic.c \
coll_han_dynamic_file.c \
coll_han_topo.c \
coll_han_subcomms.c \
coll_han_utils.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_coll_han_DSO
component_install += mca_coll_han.la
else
component_noinst += libmca_coll_han.la
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_han_la_SOURCES = $(sources)
mca_coll_han_la_LDFLAGS = -module -avoid-version
mca_coll_han_la_LIBADD =

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_han_la_SOURCES =$(sources)
libmca_coll_han_la_LDFLAGS = -module -avoid-version
