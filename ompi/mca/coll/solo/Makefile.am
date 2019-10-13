#
# Copyright (c) 2019      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        coll_solo.h \
        coll_solo_mpool.h \
        coll_solo_barrier.c \
        coll_solo_reduce.c \
        coll_solo_bcast.c \
        coll_solo_allreduce.c \
        coll_solo_component.c \
        coll_solo_module.c \
        coll_solo_mpool.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_coll_solo_DSO
component_install += mca_coll_solo.la
else
component_noinst += libmca_coll_solo.la
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_solo_la_SOURCES = $(sources)
mca_coll_solo_la_LDFLAGS = -module -avoid-version
mca_coll_solo_la_LIBADD = 

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_solo_la_SOURCES =$(sources)
libmca_coll_solo_la_LDFLAGS = -module -avoid-version
