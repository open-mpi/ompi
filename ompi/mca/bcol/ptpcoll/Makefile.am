#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2013 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

not_used_yet = 

sources = \
        bcol_ptpcoll.h \
        bcol_ptpcoll_utils.h \
        bcol_ptpcoll_utils.c \
        bcol_ptpcoll_mca.h \
        bcol_ptpcoll_mca.c \
        bcol_ptpcoll_barrier.c \
        bcol_ptpcoll_bcast.c \
        bcol_ptpcoll_bcast.h \
        bcol_ptpcoll_component.c  \
        bcol_ptpcoll_fanin.c \
        bcol_ptpcoll_fanout.c \
        bcol_ptpcoll_module.c \
        bcol_ptpcoll_allreduce.h \
        bcol_ptpcoll_allreduce.c \
        bcol_ptpcoll_reduce.h \
        bcol_ptpcoll_reduce.c \
        bcol_ptpcoll_allgather.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_bcol_ptpcoll_DSO
component_install += mca_bcol_ptpcoll.la
else
component_noinst += libmca_bcol_ptpcoll.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_bcol_ptpcoll_la_SOURCES = $(sources)
mca_bcol_ptpcoll_la_LDFLAGS = -module -avoid-version
mca_bcol_ptpcoll_la_LIBADD = 

noinst_LTLIBRARIES = $(component_noinst)
libmca_bcol_ptpcoll_la_SOURCES =$(sources)
libmca_bcol_ptpcoll_la_LDFLAGS = -module -avoid-version
